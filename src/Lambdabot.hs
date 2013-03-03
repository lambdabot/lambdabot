{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- | The guts of lambdabot.
--
-- The LB/Lambdabot monad
-- Generic server connection,disconnection
-- The module typeclass, type and operations on modules
module Lambdabot (
        Module(..), ModuleT, Mode(..),
        
        IRCRState(..), IRCRWState(..),
        LB(..), MonadLB(..),
        forkLB, liftLB,
        
        withModule, withCommand, withAllModules, getDictKeys,
        
        Command(..), runCommand, 
        Cmd, execCmd,
        
        Msg.Message(..),
        IrcMessage(..), timeReply, errShowMsg,
        send, addServer, remServer, 
        ircPrivmsg, ircPrivmsg',
        
        ircQuit, ircReconnect,
        ircGetChannels,
        ircSignalConnect, Callback, ircInstallOutputFilter, OutputFilter,
        ircInstallModule, ircUnloadModule,
        flushModuleState,
        
        checkPrivs, checkIgnore, mkCN, handleIrc, catchIrc, runIrc
  ) where

import Lambdabot.File (findLBFile)

import qualified Lambdabot.Message as Msg

import Lambdabot.Command
import Lambdabot.Config
import Lambdabot.IRC
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.State
import Lambdabot.Util.Serial
import Lambdabot.Util.Signals
import Lambdabot.Util

import Prelude hiding           (mod)

import Network                  (withSocketsDo)

import System.Exit
import System.IO

import Data.Char
import Data.List                (isSuffixOf, inits, tails)
import Data.Maybe               (isJust)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.Random.Source
import Data.Typeable

import Control.Concurrent
import Control.Exception
import qualified Control.Exception as E (catch)
import Control.Monad.Reader
import Control.Monad.State

------------------------------------------------------------------------
--
-- Lambdabot modes, networked , or command line
--
data Mode = Online | Offline deriving Eq

-- | The Lambdabot entry point.
-- Initialise plugins, connect, and run the bot in the LB monad
--
-- Also, handle any fatal exceptions (such as non-recoverable signals),
-- (i.e. print a message and exit). Non-fatal exceptions should be dealt
-- with in the mainLoop or further down.
runIrc :: LB a -> [DSum ConfigKey] -> IO ()
runIrc initialise configBindings = withSocketsDo $ do
    rost <- initRoState (config configBindings)
    r <- try $ evalLB (do withDebug "Initialising plugins" initialise
                          withIrcSignalCatch mainLoop)
                       rost initState

    -- clean up and go home
    case r of
        Left (SomeException er) -> do
            case cast er of
                Just code -> exitWith code
                Nothing -> do
                    putStrLn "exception:"
                    print er
                    exitWith (ExitFailure 1)
        Right _ -> do
            exitWith ExitSuccess

-- | Default ro state
initRoState :: Config -> IO IRCRState
initRoState configuration = do
    quitMVar     <- newEmptyMVar
    initDoneMVar <- newEmptyMVar
    
    return IRCRState 
        { ircQuitMVar       = quitMVar
        , ircInitDoneMVar   = initDoneMVar
        , ircConfig         = configuration
        }

-- | Default rw state
initState :: IRCRWState
initState = IRCRWState
    { ircPrivilegedUsers = M.singleton (Msg.Nick "offlinerc" "null") True
    , ircIgnoredUsers    = M.empty
    , ircChannels        = M.empty
    , ircModules         = M.empty
    , ircServerMap       = M.empty
    , ircCallbacks       = M.empty
    , ircOutputFilters   = 
        [ ([],cleanOutput)
        , ([],lineify)
        , ([],cleanOutput)
        -- , ([],reduceIndent)
        , ([],checkRecip) ]
    , ircCommands        = M.empty
    , ircStayConnected   = True
    }

-- Actually, this isn't a loop anymore.  FIXME: better name.
mainLoop :: LB ()
mainLoop = do
    catchIrc
        (do asks ircInitDoneMVar >>= io . flip putMVar ()
            asks ircQuitMVar >>= io . takeMVar)
        (\e -> do -- catch anything, print informative message, and clean up
            io $ hPutStrLn stderr $ case e of
                IRCRaised ex   -> "Exception: " ++ show ex
                SignalCaught s -> "Signal: " ++ ircSignalMessage s)
        
    runExitHandlers
    flushModuleState
    
    -- this kills profiling output:
    io $ exitWith (ExitFailure 1)

-- | run 'exit' handler on modules
runExitHandlers:: LB ()
runExitHandlers = withAllModules moduleExit >> return ()

-- | flush state of modules
flushModuleState :: LB ()
flushModuleState = do
    _ <- withAllModules (\m -> getModuleName >>= writeGlobalState m)
    return ()

-- ---------------------------------------------------------------------
--
-- Handling global state
--

-- | Peristence: write the global state out
writeGlobalState :: Module st -> String -> ModuleT st LB ()
writeGlobalState mod name = case moduleSerialize mod of
    Nothing  -> return ()
    Just ser -> do
        state' <- readMS
        case serialize ser state' of
            Nothing  -> return ()   -- do not write any state
            Just out -> do
                stateFile <- lb (findLBFile name)
                io (P.writeFile stateFile out)

-- | Read it in
readGlobalState :: Module st -> String -> LB (Maybe st)
readGlobalState mod name = case moduleSerialize mod of
    Just ser -> do
        stateFile <- findLBFile name
        io $ do
            state' <- Just `fmap` P.readFile stateFile `E.catch` \(_ :: SomeException) -> return Nothing
            E.catch (evaluate $ maybe Nothing (Just $!) (deserialize ser =<< state')) -- Monad Maybe)
                  (\e -> do hPutStrLn stderr $ "Error parsing state file for: "
                                            ++ name ++ ": " ++ show (e :: SomeException)
                            hPutStrLn stderr $ "Try removing: "++ show stateFile
                            return Nothing) -- proceed regardless
    Nothing -> return Nothing

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircInstallModule :: Module st -> String -> LB ()
ircInstallModule m modname = do
    savedState <- readGlobalState m modname
    state'     <- maybe (moduleDefState m) return savedState
    ref        <- io $ newMVar state'
    
    let modref = ModuleRef m ref modname
        cmdref cmd = CommandRef m ref cmd modname
    
    flip runReaderT (ref, modname) . moduleT $ do
        moduleInit m
        cmds  <- moduleCmds m
        
        s <- get
        let modmap = ircModules s
            cmdmap = ircCommands s
        put $ s {
          ircModules = M.insert modname modref modmap,
          ircCommands = M.union (M.fromList [ (name,cmdref cmd) | cmd <- cmds, name <- cmdNames cmd ]) cmdmap
        }
        io $ hPutStr stderr "." >> hFlush stderr

--
-- | Unregister a module's entry in the irc state
--
ircUnloadModule :: String -> LB ()
ircUnloadModule modname = withModule modname (error "module not loaded") (\m -> do
    when (moduleSticky m) $ error "module is sticky"
    moduleExit m
    writeGlobalState m modname
    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
        cbs    = ircCallbacks s
        svrs   = ircServerMap s
        ofs    = ircOutputFilters s
    put $ s { ircCommands      = M.filter (\(CommandRef _ _ _ name) -> name /= modname) cmdmap }
            { ircModules       = M.delete modname modmap }
            { ircCallbacks     = filter ((/=modname) . fst) `fmap` cbs }
            { ircServerMap     = M.filter ((/=modname) . fst) svrs }
            { ircOutputFilters = filter ((/=modname) . fst) ofs }
  )

------------------------------------------------------------------------

ircSignalConnect :: String -> Callback -> ModuleT mod LB ()
ircSignalConnect str f = do
    s <- get
    let cbs = ircCallbacks s
    name <- getModuleName
    case M.lookup str cbs of -- TODO
        Nothing -> put (s { ircCallbacks = M.insert str [(name,f)]    cbs})
        Just fs -> put (s { ircCallbacks = M.insert str ((name,f):fs) cbs})

ircInstallOutputFilter :: OutputFilter -> ModuleT mod LB ()
ircInstallOutputFilter f = do
    name <- getModuleName
    modify $ \s ->
        s { ircOutputFilters = (name, f): ircOutputFilters s }

-- | Checks if the given user has admin permissions and excecute the action
--   only in this case.
checkPrivs :: IrcMessage -> LB Bool
checkPrivs msg = gets (isJust . M.lookup (Msg.nick msg) . ircPrivilegedUsers)

-- | Checks if the given user is being ignored.
--   Privileged users can't be ignored.
checkIgnore :: IrcMessage -> LB Bool
checkIgnore msg = liftM2 (&&) (liftM not (checkPrivs msg))
                  (gets (isJust . M.lookup (Msg.nick msg) . ircIgnoredUsers))

------------------------------------------------------------------------
-- Some generic server operations

ircGetChannels :: LB [Msg.Nick]
ircGetChannels = (map getCN . M.keys) `fmap` gets ircChannels

-- Send a quit message, settle and wait for the server to drop our
-- handle. At which point the main thread gets a closed handle eof
-- exceptoin, we clean up and go home
ircQuit :: String -> String -> LB ()
ircQuit svr msg = do
    modify $ \state' -> state' { ircStayConnected = False }
    send  $ quit svr msg
    liftIO $ threadDelay 1000
    io $ hPutStrLn stderr "Quit"

ircReconnect :: String -> String -> LB ()
ircReconnect svr msg = do
    send $ quit svr msg
    liftIO $ threadDelay 1000

-- | Send a message to a channel\/user. If the message is too long, the rest
--   of it is saved in the (global) more-state.
ircPrivmsg :: Msg.Nick      -- ^ The channel\/user.
           -> String        -- ^ The message.
           -> LB ()

ircPrivmsg who msg = do
    filters   <- gets ircOutputFilters
    sendlines <- foldr (\f -> (=<<) (f who)) ((return . lines) msg) $ map snd filters
    mapM_ (\s -> ircPrivmsg' who (take textwidth s)) (take 10 sendlines)

-- A raw send version
ircPrivmsg' :: Msg.Nick -> String -> LB ()
ircPrivmsg' who ""  = ircPrivmsg' who " "
ircPrivmsg' who msg = send $ privmsg who msg

------------------------------------------------------------------------

-- | Print a debug message, and perform an action
withDebug :: String -> LB a -> LB ()
withDebug s a = do
    io $ hPutStr stderr (s ++ " ...")  >> hFlush stderr
    _ <- a
    io $ hPutStrLn stderr " done." >> hFlush stderr

----------------------------------------------------------------------
-- Output filters

textwidth :: Int
textwidth = 200 -- IRC maximum msg length, minus a bit for safety.

-- | For now, this just checks for duplicate empty lines.
cleanOutput :: OutputFilter
cleanOutput _ msg = return $ remDups True msg'
    where
        remDups True  ([]:xs) =    remDups True xs
        remDups False ([]:xs) = []:remDups True xs
        remDups _     (x: xs) = x: remDups False xs
        remDups _     []      = []
        msg' = map dropSpaceEnd msg

-- | wrap long lines.
lineify :: OutputFilter
lineify = const (return . mlines . unlines)

-- | break into lines
mlines :: String -> [String]
mlines = (mbreak =<<) . lines
    where
        mbreak :: String -> [String]
        mbreak xs
            | null bs   = [as]
            | otherwise = (as++cs) : filter (not . null) (mbreak ds)
            where
                (as,bs) = splitAt (w-n) xs
                breaks  = filter (not . isAlphaNum . last . fst) $ drop 1 $
                                  take n $ zip (inits bs) (tails bs)
                (cs,ds) = last $ (take n bs, drop n bs): breaks
                w = textwidth
                n = 10

-- | Don't send any output to alleged bots.
checkRecip :: OutputFilter
checkRecip who msg
--  FIXME: this doesn't work with plugin protocols :(
--  | who == Config.name Config.config                  = return []
    | "bot" `isSuffixOf` map toLower (Msg.nName who)    = return []
    | otherwise                                         = return msg

-- | Divide the lines' indent by two
{-
reduceIndent :: OutputFilter
reduceIndent _ msg = return $ map redLine msg
    where
        redLine (' ':' ':xs)        = ' ':redLine xs
        redLine xs                  = xs
-}

monadRandom [d|

    instance MonadRandom LB where
        getRandomWord8          = LB (lift getRandomWord8)
        getRandomWord16         = LB (lift getRandomWord16)
        getRandomWord32         = LB (lift getRandomWord32)
        getRandomWord64         = LB (lift getRandomWord64)
        getRandomDouble         = LB (lift getRandomDouble)
        getRandomNByteInteger n = LB (lift (getRandomNByteInteger n))

 |]
