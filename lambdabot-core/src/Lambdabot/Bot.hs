{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- | The guts of lambdabot.
--
-- The LB/Lambdabot monad
-- Generic server connection,disconnection
-- The module typeclass, type and operations on modules
module Lambdabot.Bot
    ( ircLoadModule
    , ircUnloadModule
    , ircSignalConnect
    , ircInstallOutputFilter
    , checkPrivs
    , checkIgnore
    
    , ircCodepage
    , ircGetChannels
    , ircQuit
    , ircReconnect
    , ircPrivmsg
    , ircPrivmsg'
    ) where

import Lambdabot.ChanName
import Lambdabot.Command
import Lambdabot.IRC
import Lambdabot.Logging
import Lambdabot.Message
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.Nick
import Lambdabot.OutputFilter
import Lambdabot.State
import Lambdabot.Util

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Random.Source
import qualified Data.Set as S

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircLoadModule :: Module st -> String -> LB ()
ircLoadModule m modname = do
    infoM ("Loading module " ++ show modname)
    savedState <- readGlobalState m modname
    state'     <- maybe (moduleDefState m) return savedState
    ref        <- io $ newMVar state'
    
    let modref = ModuleRef  m ref modname
        cmdref = CommandRef m ref modname
    
    mbCmds <- flip runReaderT (ref, modname) . runModuleT $ do
        initResult <- try (moduleInit m)
        case initResult of
            Left e  -> return (Left e)
            Right{} -> try (moduleCmds m)
    
    case mbCmds of
        Left e@SomeException{} -> do
            errorM ("Module " ++ show modname ++ " failed to load.  Exception thrown: " ++ show e)
            
            fail "Refusing to load due to a broken plugin"
        Right cmds -> do
            s <- get
            let modmap = ircModules s
                cmdmap = ircCommands s
            put s {
              ircModules = M.insert modname modref modmap,
              ircCommands = M.union (M.fromList [ (name,cmdref cmd) | cmd <- cmds, name <- cmdNames cmd ]) cmdmap
            }

--
-- | Unregister a module's entry in the irc state
--
ircUnloadModule :: String -> LB ()
ircUnloadModule modname = do
    infoM ("Unloading module " ++ show modname)
    
    withModule modname (error "module not loaded") $ \m -> do
        when (moduleSticky m) $ fail "module is sticky"
        
        exitResult <- try (moduleExit m)
        case exitResult of
            Right{} -> return ()
            Left e@SomeException{} -> errorM ("Module " ++ show modname ++ " threw the following exception in moduleExit: " ++ show e)
        
        writeGlobalState m modname
    
    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
        cbs    = ircCallbacks s
        svrs   = ircServerMap s
        ofs    = ircOutputFilters s
    put s
        { ircCommands      = M.filter (\(CommandRef _ _ name _) -> name /= modname) cmdmap
        , ircModules       = M.delete modname modmap
        , ircCallbacks     =   filter ((/=modname) . fst) `fmap` cbs
        , ircServerMap     = M.filter ((/=modname) . fst) svrs
        , ircOutputFilters =   filter ((/=modname) . fst) ofs
        }

------------------------------------------------------------------------

ircSignalConnect :: String -> Callback -> ModuleT mod LB ()
ircSignalConnect str f = do
    s <- lift get
    let cbs = ircCallbacks s
    name <- getModuleName
    case M.lookup str cbs of -- TODO: figure out what this TODO is for
        Nothing -> lift (put s { ircCallbacks = M.insert str [(name,f)]    cbs})
        Just fs -> lift (put s { ircCallbacks = M.insert str ((name,f):fs) cbs})

ircInstallOutputFilter :: OutputFilter LB -> ModuleT mod LB ()
ircInstallOutputFilter f = do
    name <- getModuleName
    lift . modify $ \s ->
        s { ircOutputFilters = (name, f): ircOutputFilters s }

-- | Checks if the given user has admin permissions and excecute the action
--   only in this case.
checkPrivs :: IrcMessage -> LB Bool
checkPrivs msg = gets (S.member (nick msg) . ircPrivilegedUsers)

-- | Checks if the given user is being ignored.
--   Privileged users can't be ignored.
checkIgnore :: IrcMessage -> LB Bool
checkIgnore msg = liftM2 (&&) (liftM not (checkPrivs msg))
                  (gets (S.member (nick msg) . ircIgnoredUsers))

------------------------------------------------------------------------
-- Some generic server operations

-- Send a CODEPAGE command to set encoding for current session.
-- Some IRC networks don't provide UTF-8 ports, but allow
-- switching it in runtime
ircCodepage :: String -> String -> LB ()
ircCodepage svr cpage = do
    send $ codepage svr cpage

ircGetChannels :: LB [Nick]
ircGetChannels = (map getCN . M.keys) `fmap` gets ircChannels

-- Send a quit message, settle and wait for the server to drop our
-- handle. At which point the main thread gets a closed handle eof
-- exceptoin, we clean up and go home
ircQuit :: String -> String -> LB ()
ircQuit svr msg = do
    modify $ \state' -> state' { ircPersists = M.delete svr $ ircPersists state' }
    send  $ quit svr msg
    liftIO $ threadDelay 1000
    noticeM "Quitting"

ircReconnect :: String -> String -> LB ()
ircReconnect svr msg = do
    modify $ \state' -> state' { ircPersists = M.insertWith (flip const) svr False $ ircPersists state' }
    send $ quit svr msg
    liftIO $ threadDelay 1000

-- | Send a message to a channel\/user. If the message is too long, the rest
--   of it is saved in the (global) more-state.
ircPrivmsg :: Nick      -- ^ The channel\/user.
           -> String        -- ^ The message.
           -> LB ()

ircPrivmsg who msg = do
    filters   <- gets ircOutputFilters
    sendlines <- foldr (\f -> (=<<) (f who)) ((return . lines) msg) $ map snd filters
    mapM_ (\s -> ircPrivmsg' who (take textwidth s)) (take 10 sendlines)

-- A raw send version
ircPrivmsg' :: Nick -> String -> LB ()
ircPrivmsg' who ""  = ircPrivmsg' who " "
ircPrivmsg' who msg = send $ privmsg who msg

------------------------------------------------------------------------

monadRandom [d|

    instance MonadRandom LB where
        getRandomWord8          = LB (lift getRandomWord8)
        getRandomWord16         = LB (lift getRandomWord16)
        getRandomWord32         = LB (lift getRandomWord32)
        getRandomWord64         = LB (lift getRandomWord64)
        getRandomDouble         = LB (lift getRandomDouble)
        getRandomNByteInteger n = LB (lift (getRandomNByteInteger n))

 |]
