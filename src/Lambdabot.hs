{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
-- | The guts of lambdabot.
--
-- The LB/Lambdabot monad
-- Generic server connection,disconnection
-- The module typeclass, type and operations on modules
module Lambdabot (
        MODULE(..), Module(..), lookupCmd,
        ModuleT, Mode(..),

        IRCRState(..), IRCRWState(..), IRCError(..),

        LB(..), MonadLB(..), lbIO,

        withModule, withAllModules, getDictKeys,

        getRef, getName, bindModule0, bindModule1, bindModule2,

        send, addServer, remServer, addServer',
        ircPrivmsg, ircPrivmsg', -- not generally used
        ircPrivmsgF,

        ircQuit, ircReconnect,
        ircGetChannels,
        ircSignalConnect, Callback, ircInstallOutputFilter, OutputFilter,
        ircInstallModule, ircUnloadModule,
        flushModuleState,

        ircLoad, ircUnload,

        checkPrivs, checkIgnore, mkCN, handleIrc, catchIrc, runIrc,
        
        forkLB, liftLB
  ) where

import Lambdabot.File (findFile)

import qualified Lambdabot.Command as Cmd
import qualified Lambdabot.Message as Msg
import qualified Lambdabot.Shared  as S
import qualified Lambdabot.IRC as IRC (IrcMessage, quit, privmsg)

import Lambdabot.Monad
import Lambdabot.Module
import Lambdabot.Signals
import Lambdabot.Util
import Lambdabot.Serial

import Prelude hiding           (mod, catch)

import Network                  (withSocketsDo)

import System.Exit
import System.IO
import System.IO.Unsafe

#ifndef mingw32_HOST_OS
-- n.b comment this out for prof
import System.Posix.Process     ( exitImmediately )
#endif

import Data.Char
import Data.List                (isSuffixOf, inits, tails)
import Data.Maybe               (isJust)
import Data.Map (Map)
import qualified Data.Map as M hiding (Map)
import qualified Data.ByteString.Char8 as P
import Data.ByteString (ByteString)

import Control.Concurrent (myThreadId, newEmptyMVar, newMVar, readMVar, putMVar,
                           takeMVar, threadDelay)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

#ifdef mingw32_HOST_OS
-- compatability shim
exitImmediately :: ExitCode -> IO a
exitImmediately = exitWith
#endif

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
runIrc :: [String] -> LB a -> S.DynLoad -> [String] -> IO ()
runIrc evcmds initialise ld plugins = withSocketsDo $ do
    rost <- initRoState
    r <- try $ evalLB (do withDebug "Initialising plugins" initialise
                          withIrcSignalCatch mainLoop)
                       rost (initState ld plugins evcmds)

    -- clean up and go home
    case r of
        Left er -> do putStrLn "exception:"
                      print (er :: SomeException)
                      exitWith (ExitFailure 1) -- won't happen.  exitImmediately cleans it all up
        Right _ -> exitWith ExitSuccess

--
-- | Default ro state
--
initRoState :: IO IRCRState
initRoState = do
    threadmain <- io myThreadId
    quitMVar <- io newEmptyMVar
    initDoneMVar <- io newEmptyMVar

    return $ IRCRState {
                 ircQuitMVar    = quitMVar,
                 ircInitDoneMVar= initDoneMVar,
                 ircMainThread  = threadmain
             }

--
-- | Default rw state
--
initState :: S.DynLoad -> [String] -> [String] -> IRCRWState
initState ld plugins evcmds = IRCRWState {
        ircPrivilegedUsers = M.singleton (Msg.Nick "offlinerc" "null") True,
        ircIgnoredUsers    = M.empty,
        ircChannels        = M.empty,
        ircModules         = M.empty,
        ircServerMap       = M.empty,
        ircCallbacks       = M.empty,
        ircOutputFilters   = [
            ([],cleanOutput),
            ([],lineify),
            ([],cleanOutput),
        --  ([],reduceIndent),
            ([],checkRecip) ],
        ircCommands        = M.empty,
        ircPrivCommands    = [],
        ircStayConnected   = True,
        ircDynLoad         = ld,
        ircPlugins         = plugins,
        ircOnStartupCmds   = evcmds
    }

-- Actually, this isn't a loop anymore.  FIXME: better name.
mainLoop :: LB ()
mainLoop = catchIrc
   (do asks ircInitDoneMVar >>= io . flip putMVar ()
       asks ircQuitMVar >>= io . takeMVar
       fail "don't write to the quitMVar!")
   (\e -> do -- catch anything, print informative message, and clean up
        io $ hPutStrLn stderr $ case e of
            IRCRaised ex   -> "Exception: " ++ show ex
            SignalCaught s -> "Signal: " ++ ircSignalMessage s
        
        runExitHandlers
        flushModuleState
        
        -- this kills profiling output:
        io $ exitImmediately (ExitFailure 1))

-- | run 'exit' handler on modules
runExitHandlers:: LB ()
runExitHandlers = withAllModules (const moduleExit) >> return ()

-- | flush state of modules
flushModuleState :: LB ()
flushModuleState = do
    withAllModules (\m -> getName >>= writeGlobalState m)
    return ()

-- ---------------------------------------------------------------------
--
-- Handling global state
--

-- | Peristence: write the global state out
writeGlobalState :: Module m => m -> String -> ModuleT m LB ()
writeGlobalState mod name = case moduleSerialize mod of
  Nothing  -> return ()
  Just ser -> do
    state <- getRef >>= (io . readMVar) -- readMS
    case serialize ser state of
        Nothing  -> return ()   -- do not write any state
        Just out -> io $ P.writeFile (toFilename name) out

-- | Read it in
readGlobalState :: Module m => m -> String -> IO (Maybe (ModuleState m))
readGlobalState mod name = case moduleSerialize mod of
    Just ser -> do
        state <- Just `fmap` P.readFile (toFilename name) `catch` \(_ :: SomeException) -> return Nothing
        catch (evaluate $ maybe Nothing (Just $!) (deserialize ser =<< state)) -- Monad Maybe)
              (\e -> do hPutStrLn stderr $ "Error parsing state file for: "
                                        ++ name ++ ": " ++ show (e :: SomeException)
                        hPutStrLn stderr $ "Try removing: "++ show (toFilename name)
                        return Nothing) -- proceed regardless
    Nothing -> return Nothing

-- | helper
toFilename :: String -> String
toFilename = unsafePerformIO . findFile

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircInstallModule :: MODULE -> String -> LB ()
ircInstallModule (MODULE (mod :: m)) modname = do
    savedState <- io $ readGlobalState mod modname
    state      <- maybe (moduleDefState mod) return savedState
    ref        <- io $ newMVar state
    
    let modref = ModuleRef mod ref modname
    
    flip runReaderT (ref, modname) . moduleT $ do
        moduleInit :: ModuleT m LB ()
        cmds  <- moduleCmds
        
        s <- get
        let modmap = ircModules s
            cmdmap = ircCommands s
        put $ s {
          ircModules = M.insert modname modref modmap,
          ircCommands = addList [ (name,modref) | cmd <- cmds, name <- Cmd.cmdNames cmd ] cmdmap,
          ircPrivCommands = ircPrivCommands s ++ concatMap Cmd.cmdNames (filter Cmd.privileged cmds)
        }
        io $ hPutStr stderr "." >> hFlush stderr

--
-- | Unregister a module's entry in the irc state
--
ircUnloadModule :: String -> LB ()
ircUnloadModule modname = withModule ircModules modname (error "module not loaded") (\m -> do
    when (moduleSticky m) $ error "module is sticky"
    moduleExit
    writeGlobalState m modname
    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
        cbs    = ircCallbacks s
        svrs   = ircServerMap s
        ofs    = ircOutputFilters s
    put $ s { ircCommands      = M.filter (\(ModuleRef _ _ name) -> name /= modname) cmdmap }
            { ircModules       = M.delete modname modmap }
            { ircCallbacks     = filter ((/=modname) . fst) `fmap` cbs }
            { ircServerMap     = M.filter ((/=modname) . fst) svrs }
            { ircOutputFilters = filter ((/=modname) . fst) ofs }
  )

--
-- | Binding to dynamic loader functions (stored as a bundle in state)
-- passed from Boot. DynamicModule goes through here to get at them.
--
ircLoad :: FilePath -> S.Symbol -> LB (S.Module, a)
ircLoad mod sym = do
    s <- get
    let fn  = S.dynload (ircDynLoad s)
    io $ (fn mod sym)

--
-- | Dynamically unload a module
--
ircUnload :: FilePath -> LB ()
ircUnload mod = do
    s <- get
    io $ (S.unload (ircDynLoad s)) (S.Module mod)

------------------------------------------------------------------------

ircSignalConnect :: String -> Callback -> ModuleT mod LB ()
ircSignalConnect str f = do
    s <- get
    let cbs = ircCallbacks s
    name <- getName
    case M.lookup str cbs of -- TODO
        Nothing -> put (s { ircCallbacks = M.insert str [(name,f)]    cbs})
        Just fs -> put (s { ircCallbacks = M.insert str ((name,f):fs) cbs})

ircInstallOutputFilter :: OutputFilter -> ModuleT mod LB ()
ircInstallOutputFilter f = do
    name <- getName
    modify $ \s ->
        s { ircOutputFilters = (name, f): ircOutputFilters s }

-- | Checks if the given user has admin permissions and excecute the action
--   only in this case.
checkPrivs :: IRC.IrcMessage -> LB Bool
checkPrivs msg = gets (isJust . M.lookup (Msg.nick msg) . ircPrivilegedUsers)

-- | Checks if the given user is being ignored.
--   Privileged users can't be ignored.
checkIgnore :: IRC.IrcMessage -> LB Bool
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
    modify $ \state -> state { ircStayConnected = False }
    send  $ IRC.quit svr msg
    liftIO $ threadDelay 1000
    io $ hPutStrLn stderr "Quit"

ircReconnect :: String -> String -> LB ()
ircReconnect svr msg = do
    send $ IRC.quit svr msg
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
ircPrivmsg' who msg = send $ IRC.privmsg who msg

----------------------------------------------------------------------------------

ircPrivmsgF :: Msg.Nick -> ByteString -> LB ()
ircPrivmsgF who s= ircPrivmsg who (P.unpack s) -- TODO

{-
rawPrivmsgF :: String -> Maybe ByteString -> LB ()
rawPrivmsgF _   Nothing  = send Nothing
rawPrivmsgF who (Just s) | P.null s  = ircPrivmsg' who (Just " ")
                         | otherwise = send . Just $ IRC.privmsgF who msg
-}

------------------------------------------------------------------------
-- Module handling

-- | Interpret an expression in the context of a module.
-- Arguments are which map to use (@ircModules@ and @ircCommands@ are
-- the only sensible arguments here), the name of the module\/command,
-- action for the case that the lookup fails, action if the lookup
-- succeeds.
--
withModule :: (Ord k)
           => (IRCRWState -> Map k ModuleRef)
           -> k
           -> LB a
           -> (forall mod. Module mod => mod -> ModuleT mod LB a)
           -> LB a

withModule dict modname def f = do
    maybemod <- gets (M.lookup modname . dict)
    case maybemod of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (ModuleRef m ref name) -> do
          runReaderT (moduleT $ f m) (ref, name)
      _                           -> def

-- | Interpret a function in the context of all modules
withAllModules :: (forall mod. Module mod => mod -> ModuleT mod LB a) -> LB [a]
withAllModules f = do
    mods <- gets $ M.elems . ircModules :: LB [ModuleRef]
    (`mapM` mods) $ \(ModuleRef m ref name) -> do
        runReaderT (moduleT $ f m) (ref, name)

getDictKeys :: (MonadState s m) => (s -> Map k a) -> m [k]
getDictKeys dict = gets (M.keys . dict)

------------------------------------------------------------------------

-- | Print a debug message, and perform an action
withDebug :: String -> LB a -> LB ()
withDebug s a = do
    io $ hPutStr stderr (s ++ " ...")  >> hFlush stderr
    a
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
--  | who == Config.name Config.config                   = return []
    | "bot" `isSuffixOf` lowerCaseString (Msg.nName who) = return []
    | otherwise                                          = return msg

-- | Divide the lines' indent by two
{-
reduceIndent :: OutputFilter
reduceIndent _ msg = return $ map redLine msg
    where
        redLine (' ':' ':xs)        = ' ':redLine xs
        redLine xs                  = xs
-}
