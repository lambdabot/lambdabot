{-# OPTIONS -cpp #-}
--
-- | The guts of lambdabot.
--
-- The LB/Lambdabot monad
-- Generic server connection,disconnection
-- Signal and exception handling
-- The module typeclass, type and operations on modules
--
module Lambdabot (
        MODULE(..), Module(..),
        ModuleT, ModState, ModuleLB, Mode(..),

        IRCRState(..), IRCRWState(..), IRCError(..),
        module Msg,

        LB, liftLB, lbIO,

        withModule, withAllModules, getDictKeys,

        send, send_,
        ircPrivmsg, ircPrivmsg', -- not generally used
        ircQuit, ircReconnect,
        ircGetChannels,
        ircSignalConnect, Callback, ircInstallOutputFilter, OutputFilter,
        ircInstallModule, ircUnloadModule,
        serverSignOn,
        ircRead,

        ircLoad, ircUnload,

        checkPrivs, mkCN, handleIrc, catchIrc, runIrc,

        io,
  ) where

import qualified Config (config, name, admins, host, port, Protocol(..))
import qualified Message as Msg
import qualified IRC    (IrcMessage, quit, privmsg, readerLoop
                        ,writerLoop, offlineReaderLoop, offlineWriterLoop, user, setNick)
import qualified Shared as S
import ErrorUtils

import Lib.Util             (lowerCaseString, addList,dropSpace)
import Lib.Serial

import Data.Map (Map)
import qualified Data.Map as M hiding (Map)
import qualified Data.ByteString.Char8 as P

import Prelude hiding   (mod, catch)

import Network          (withSocketsDo, connectTo, PortID(PortNumber))

import System.IO
import System.Exit

# ifndef mingw32_HOST_OS
import System.Posix.Process
import System.Posix.Signals
import System.IO.Unsafe         (unsafePerformIO)
# endif

import Data.Char                (toLower, isAlphaNum, isSpace)
import Data.List                (isSuffixOf, inits, tails)
import Data.Typeable            (Typeable)
import Data.IORef               (newIORef, IORef, readIORef, writeIORef)
import Data.Maybe               (isJust)

import Control.Exception
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error (MonadError (..))
import Control.Monad.Trans      ( liftIO )

#if __GLASGOW_HASKELL__ >= 605
import GHC.Err
#endif

------------------------------------------------------------------------

#ifdef mingw32_HOST_OS
type Signal = ()
type Handler = ()
type SignalSet = ()
#endif

-- | Global read-only state.
data IRCRState
  = IRCRState {
        ircServer      :: String,
        ircReadChan    :: Pipe,
        ircReadThread  :: ThreadId,
        ircWriteChan   :: Pipe,
        ircWriteThread :: ThreadId
  }

type Pipe = Chan (Maybe IRC.IrcMessage)

type Callback = IRC.IrcMessage -> LB ()

type OutputFilter = String -> [String] -> LB [String]

-- | Global read\/write state.
data IRCRWState = IRCRWState {
    ircPrivilegedUsers :: Map String Bool,

    ircChannels        :: Map ChanName String,
        -- ^ maps channel names to topics

    ircModules         :: Map String ModuleRef,
    ircCallbacks       :: Map String [(String,Callback)],
    ircOutputFilters   :: [(String,OutputFilter)],
        -- ^ Output filters, invoked from right to left

    ircCommands        :: Map String ModuleRef,
    ircPrivCommands    :: [String],
    ircStayConnected   :: Bool,
    ircDynLoad         :: S.DynLoad
}

newtype ChanName = ChanName { getCN :: String } -- should be abstract, always lowercase
  deriving (Eq, Ord)

instance Show ChanName where
  show (ChanName x) = show x

-- | only use the "smart constructor":
mkCN :: String -> ChanName
mkCN = ChanName . map toLower

-- ---------------------------------------------------------------------
--
-- The LB monad
--

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
newtype LB a = LB { runLB :: ReaderT (IORef (Maybe IRCRState),IORef IRCRWState) IO a }

#ifndef __HADDOCK__
  deriving (Monad,Functor,MonadIO)
#endif

-- | lift an io transformer into LB
liftLB :: (IO a -> IO b) -> LB a -> LB b
liftLB f = LB . mapReaderT f . runLB -- lbIO (\conv -> f (conv lb))

-- lbIO return :: LB (LB a -> IO a)
-- CPS to work around predicativiy of haskell's type system.
lbIO :: ((forall a. LB a -> IO a) -> IO b) -> LB b
lbIO k = LB . ReaderT $ \r -> k (\(LB m) -> m `runReaderT` r)

-- Actually, this isn't a reader anymore
instance MonadReader IRCRState LB where
  ask   = LB $ fmap (maybe (error "No connection") id) $ io . readIORef =<< asks fst
  local = error "You are not supposed to call local"

-- | Take a state, and a lambdabot monad action, run the action,
-- preserving the state before and after you run the action.
localLB :: Maybe IRCRState -> LB a -> LB a
localLB new (LB m) = LB $ do
    ref <- asks fst
    old <- io $ readIORef ref
    io $ writeIORef ref new
    res <- m
    io $ writeIORef ref old
    return res

instance MonadState IRCRWState LB where
    get = LB $ do
        ref <- asks snd
        lift $ readIORef ref
    put x = LB $ do
        ref <- asks snd
        lift $ writeIORef ref x

-- | run a computation in the LB monad
evalLB :: LB a -> IRCRWState -> IO a
evalLB (LB lb) rws = do
    ref  <- newIORef rws
    ref' <- newIORef Nothing
    lb `runReaderT` (ref',ref)

------------------------------------------------------------------------
-- The signal story.
--
-- Posix signals are external events that invoke signal handlers in
-- Haskell. The signal handlers in turn throw dynamic exceptions.
-- Our instance of MonadError for LB maps the dynamic exceptions to
-- SignalCaughts, which can then be caught by a normal catchIrc or
-- handleIrc
--
-- Here's where we do that.
--

-- A new type for the SignalException, must be Typeable so we can make a
-- dynamic exception out of it.
newtype SignalException = SignalException Signal deriving Typeable

--
-- A bit of sugar for installing a new handler
-- 
withHandler :: (MonadIO m,MonadError e m) => Signal -> Handler -> m () -> m ()
#ifdef mingw32_HOST_OS
withHandler s h m = return ()
#else
withHandler s h m
  = bracketError (io (installHandler s h Nothing))
                 (io . flip (installHandler s) Nothing)
                 (const m)
#endif

-- And more sugar for installing a list of handlers
withHandlerList :: (MonadError e m,MonadIO m)
                => [Signal] -> (Signal -> Handler) -> m () -> m ()
withHandlerList sl h m = foldr (withHandler `ap` h) m sl

--
-- Signals we care about. They're all fatal.
--
-- Be careful adding signals, some signals can't be caught and
-- installHandler just raises an exception if you try
--
ircSignalsToCatch :: [Signal]
ircSignalsToCatch = [
#ifndef mingw32_HOST_OS
    busError,
    segmentationViolation,
    keyboardSignal,
    softwareTermination,
    keyboardTermination,
    lostConnection,
    internalAbort
#endif
    ]

--
-- User friendly names for the signals that we can catch
--
ircSignalMessage :: Signal -> [Char]
ircSignalMessage s
#ifndef mingw32_HOST_OS
   | s==busError              = "SIGBUS"
   | s==segmentationViolation = "SIGSEGV"
   | s==keyboardSignal        = "SIGINT"
   | s==softwareTermination   = "SIGTERM"
   | s==keyboardTermination   = "SIGQUIT"
   | s==lostConnection        = "SIGHUP"
   | s==internalAbort         = "SIGABRT"
#endif
-- this case shouldn't happen if the list of messages is kept up to date
-- with the list of signals caught
   | otherwise                  = "killed by unknown signal"

--
-- The actual signal handler. It is this function we register for each
-- signal flavour. On receiving a signal, the signal handler maps the
-- signal to a a dynamic exception, and throws it out to the main
-- thread. The LB MonadError instance can then do its trickery to catch
-- it in handler/catchIrc
--
ircSignalHandler :: ThreadId -> Signal -> Handler
ircSignalHandler threadid s
#ifdef mingw32_HOST_OS
  = ()
#else
  = CatchOnce $ do
        putMVar catchLock ()
        releaseSignals
        throwDynTo threadid $ SignalException s

--
-- | Release all signal handlers
--
releaseSignals :: IO ()
releaseSignals =
    flip mapM_ ircSignalsToCatch
               (\sig -> installHandler sig Default Nothing)

--
-- Mututally exclusive signal handlers
--
-- This is clearly a hack, but I have no idea how to accomplish the same
-- thing correctly. The main problem is that signals are often thrown
-- multiple times, and the threads start killing each other if we allow
-- the SignalException to be thrown more than once.
{-# NOINLINE catchLock #-}
catchLock :: MVar ()
catchLock = unsafePerformIO newEmptyMVar
#endif

--
-- | Register signal handlers to catch external signals
--
withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m = do
    io $ installHandler sigPIPE Ignore Nothing
    io $ installHandler sigALRM Ignore Nothing
    threadid <- io myThreadId
    withHandlerList ircSignalsToCatch (ircSignalHandler threadid) m

--
-- A type for handling both Haskell exceptions and external signals
--
data IRCError = IRCRaised Exception | SignalCaught Signal deriving Show

--
-- And now a MonadError instance to map IRCErrors to MonadError in LB,
-- so throwError and catchError "just work"
--
instance MonadError IRCError LB where
  throwError (IRCRaised e)    = io $ throwIO e
  throwError (SignalCaught e) = io $ evaluate (throwDyn $ SignalException e)
  m `catchError` h = lbIO $ \conv -> (conv m
              `catchDyn` \(SignalException e) -> conv $ h $ SignalCaught e)
              `catch` \e -> conv $ h $ IRCRaised e

-- ---------------------------------------------------------------------

--
-- May wish to add more things to the things caught, or restructure things 
-- a bit. Can't just catch everything - in particular EOFs from the socket
-- loops get thrown to this thread and we musn't just ignore them.
--
handleIrc :: MonadError IRCError m => (IRCError -> m ()) -> m () -> m ()
handleIrc handler m = catchError m handler

-- Like handleIrc, but with arguments reversed
catchIrc :: MonadError IRCError m => m () -> (IRCError -> m ()) -> m ()
catchIrc = flip handleIrc

------------------------------------------------------------------------
--
-- Lambdabot modes, networked , or command line
--
data Mode = Online | Offline deriving Eq

--
-- | The Lambdabot entry point.
-- Initialise plugins, connect, and run the bot in the LB monad
--
-- Also, handle any fatal exceptions (such as non-recoverable signals),
-- (i.e. print a message and exit). Non-fatal exceptions should be dealt
-- with in the mainLoop or further down.
--
runIrc :: Mode -> LB a -> LB () -> S.DynLoad -> IO ()
runIrc mode initialise loop ld = withSocketsDo $ do
    r <- try $ evalLB (do withDebug "Initialising plugins" initialise
                          withIrcSignalCatch (mainLoop mode loop))
                       (initState (Config.admins Config.config) ld)

    -- clean up and go home
    case r of
        Left _  -> exitWith (ExitFailure 1) -- won't happen.  exitImmediately cleans it all up
        Right _ -> exitWith ExitSuccess

--
-- | Default rw state
--
initState :: [String] -> S.DynLoad -> IRCRWState
initState as ld = IRCRWState {
        ircPrivilegedUsers = M.fromList $ zip as (repeat True),
        ircChannels        = M.empty,
        ircModules         = M.empty,
        ircCallbacks       = M.empty,
        ircOutputFilters   = [
            ("",cleanOutput),
            ("",lineify),
            ("",cleanOutput),
            ("",reduceIndent),
            ("",checkRecip) ],
        ircCommands        = M.empty,
        ircPrivCommands    = [],
        ircStayConnected   = True,
        ircDynLoad         = ld
    }

------------------------------------------------------------------------

--
-- Actually connect to the irc server
--
mainLoop :: Mode -> LB a -> LB ()
mainLoop mode loop = do

    -- in offline mode we connect to stdin/stdout. in online mode our
    -- handles are network pipes
    (hin,hout,rloop,wloop) <- case mode of
        Online  -> do
            let portnum  = PortNumber $ fromIntegral (Config.port Config.config)
            s <- io $ connectTo (Config.host Config.config) portnum
            return (s,s,IRC.readerLoop,IRC.writerLoop)
        Offline -> return (stdin, stdout, IRC.offlineReaderLoop, IRC.offlineWriterLoop)

    io $ hSetBuffering hout NoBuffering
    io $ hSetBuffering hin  NoBuffering
    threadmain <- io myThreadId
    chanr      <- io newChan
    chanw      <- io newChan
    syncR      <- io $ newMVar () -- used in offline to make threads synchronous
    syncW      <- io newEmptyMVar
    threadr    <- io $ forkIO $ rloop threadmain chanr chanw hin syncR syncW
    threadw    <- io $ forkIO $ wloop threadmain chanw hout syncR syncW

    let chans = IRCRState {
                    ircServer      = Config.host Config.config,
                    ircReadChan    = chanr,
                    ircReadThread  = threadr,
                    ircWriteChan   = chanw,
                    ircWriteThread = threadw
                }

    catchIrc (localLB (Just chans) loop >> return ())

        -- catch anything, print informative message, and clean up
       (\e -> do
            io $ hPutStrLn stderr $
                       (case e of
                            IRCRaised ex   -> "Exception: " ++ show ex
                            SignalCaught s -> "Signal: " ++ ircSignalMessage s)
            runExitHandlers
            withDebug "Writing persistent state" flushModuleState
            io $ do hPutStrLn stderr "Exiting ... "
                    exitImmediately (ExitFailure 1))
      --    throwError e)

-- | run 'exit' handler on modules
runExitHandlers:: LB ()
runExitHandlers = withAllModules moduleExit>> return ()

-- | flush state of modules
flushModuleState :: LB ()
flushModuleState = withAllModules (\m -> writeGlobalState m ?name) >> return ()

------------------------------------------------------------------------

-- | The Module type class.
-- Minimal complete definition: @moduleHelp@, @moduleCmds@, and 
-- either @process@ or @process_@
class Module m s | m -> s where
    -- | If the module wants its state to be saved, this function should
    --   return a Serial.
    --
    --   The default implementation returns Nothing.
    moduleSerialize :: m -> Maybe (Serial s)

    -- | If the module maintains state, this method specifies the default state
    --   (for example in case the state can't be read from a state).
    --
    --   The default implementation returns an error and assumes the state is 
    --   never accessed.
    moduleDefState  :: m -> LB s

    -- | Is the module sticky? Sticky modules (as well as static ones) can't be
    --   unloaded. By default, modules are not sticky.
    moduleSticky    :: m -> Bool

    -- | The commands the module listenes to.
    moduleCmds      :: m -> [String]

    -- | This method should return a help string for every command it defines.
    moduleHelp      :: m -> String -> String

    -- | The privileged commands the module listenes to.
    modulePrivs     :: m -> [String]

    -- | Initialize the module. The default implementation does nothing.
    moduleInit      :: m -> ModuleT s LB ()

    -- | Finalize the module. The default implementation does nothing.
    moduleExit      :: m -> ModuleT s LB ()

    -- | Process a command a user sent, the resulting string is draw in
    -- some fashion. If the `process' function doesn't exist, we catch
    -- an exception when we try to call it, and instead call `process_'
    -- which is guaranteed to at least have a default instance.
    -- This magic (well, for Haskell) occurs in Base.hs
    --
    process :: Msg.Message a
        => m                                -- ^ phantom     (required)
        -> a                                -- ^ the message (uneeded by most?)
        -> String                           -- ^ target
        -> String                           -- ^ command
        -> String                           -- ^ the arguments to the command
        -> ModuleLB s                       -- ^ maybe output

    -- | Process contextual input. A plugin that implements 'contextual'
    -- is able to respond to text not part of a normal command.
    contextual :: Msg.Message a
        => m                                -- ^ phantom     (required)
        -> a                                -- ^ the message
        -> String                           -- ^ target
        -> String                           -- ^ the text
        -> ModuleLB s                       -- ^ maybe output

    -- | Like process, but uncommonly used args are ignored
    -- Lambdabot will attempt to run process first, and then fall back
    -- to process_, which in turn has a default instance.
    --
    process_ :: m                           -- ^ phantom
             -> String -> String            -- ^ command, args
             -> ModuleLB s                  -- ^ maybe output

------------------------------------------------------------------------

    contextual _ _ _ _ = return []
    process_ _ _ _     = return []

    moduleHelp m _     = concat (map ('@':) (moduleCmds m))
    modulePrivs _      = []
    moduleCmds      _  = []
    moduleExit _       = return ()
    moduleInit _       = return ()
    moduleSticky _     = False
    moduleSerialize _  = Nothing
    moduleDefState  _  = return $ error "state not initalized"

-- work around weird issue in 6.5, where the missing default fails
#if __GLASGOW_HASKELL__ >= 605
    process _ _ _ _ _ = GHC.Err.noMethodBindingError "Lambdabot.process"#
#endif

------------------------------------------------------------------------

-- | An existential type holding a module, used to represent modules on
-- the value level, for manipluation at runtime by the dynamic linker.
--
data MODULE = forall m s. (Module m s) => MODULE m

data ModuleRef = forall m s. (Module m s) => ModuleRef m (MVar s) String

--
-- | This \"transformer\" encodes the additional information a module might 
--   need to access its name or its state.
--
-- TODO: remove implicit parameters. It won't be valid Haskell'
--
type ModuleT s m a = (?ref :: MVar s, ?name :: String) => m a

-- Name !!!
type ModState s a = (?ref :: MVar s, ?name :: String) => a

-- | A nicer synonym for some ModuleT stuffs
type ModuleLB m = ModuleT m LB [String]

------------------------------------------------------------------------

-- | Peristence: write the global state out
--
writeGlobalState :: Module m s => m -> String -> ModuleT s LB ()
writeGlobalState mod name = case moduleSerialize mod of
  Nothing  -> return ()
  Just ser -> do
    state <- io $ readMVar ?ref -- readMS
    case serialize ser state of
        Nothing  -> return ()   -- do not write any state
        Just out -> io $ P.writeFile (toFilename name) out

readFile' :: String -> IO P.ByteString
readFile' = P.readFile
{-# INLINE readFile' #-}

readGlobalState :: Module m s => m -> String -> IO (Maybe s)
readGlobalState mod name = 
  case moduleSerialize mod of
          Nothing  -> return Nothing
          Just ser -> do
            state  <- Just `fmap` readFile' (toFilename name) `catch` \_ -> return Nothing
            state' <- {-# SCC "readGlobalState.1" #-} evaluate $ deserialize ser =<< state
            return $! maybe Nothing (Just $!) $ state'
{-# INLINE readGlobalState #-}

-- | helper
toFilename :: String -> String
toFilename = ("State/"++)

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircInstallModule :: MODULE -> String -> LB ()
ircInstallModule (MODULE mod) modname = do
    savedState <- io $ readGlobalState mod modname
    state      <- maybe (moduleDefState mod) return savedState
    ref        <- io $ newMVar state

    let modref = ModuleRef mod ref modname

    -- TODO
    let ?ref = ref; ?name = modname -- yikes
    moduleInit mod
    let cmds  = moduleCmds mod
        privs = modulePrivs mod

    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
    put $ s {
      ircModules = M.insert modname modref modmap,
      ircCommands = addList [ (cmd,modref) | cmd <- cmds++privs ] cmdmap,
      ircPrivCommands = ircPrivCommands s ++ privs
    }
    io $ hPutStr stderr "." >> hFlush stderr

--
-- | Unregister a module's entry in the irc state
--
ircUnloadModule :: String -> LB ()
ircUnloadModule modname = withModule ircModules modname (error "module not loaded") (\m -> do
    when (moduleSticky m) $ error "module is sticky"
    moduleExit m
    writeGlobalState m modname
    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
        cbs    = ircCallbacks s
        ofs    = ircOutputFilters s
    put $ s { ircCommands = M.filter (\(ModuleRef _ _ name) -> name /= modname) cmdmap }
            { ircModules = M.delete modname modmap }
            { ircCallbacks = filter ((/=modname) . fst) `fmap` cbs }
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

ircSignalConnect :: String -> Callback -> ModuleT s LB ()
ircSignalConnect str f 
    = do s <- get
         let cbs = ircCallbacks s
         case M.lookup str cbs of 
              -- TODO
              Nothing -> put (s { ircCallbacks = M.insert str [(?name,f)]    cbs}) 
              Just fs -> put (s { ircCallbacks = M.insert str ((?name,f):fs) cbs}) 

ircInstallOutputFilter :: OutputFilter -> ModuleT s LB ()
ircInstallOutputFilter f = modify $ \s -> 
  -- TODO
  s { ircOutputFilters = (?name, f): ircOutputFilters s }

-- | Checks if the given user has admin permissions and excecute the action
--   only in this case.
checkPrivs :: IRC.IrcMessage -> LB Bool
checkPrivs msg = gets (isJust . M.lookup (Msg.nick msg) . ircPrivilegedUsers)

------------------------------------------------------------------------
-- Some generic server operations

serverSignOn :: Config.Protocol -> String -> String -> LB ()
serverSignOn Config.Irc  nick userinfo = ircSignOn nick userinfo
serverSignOn Config.Xmpp nick _        = jabberSignOn nick

jabberSignOn :: String -> LB ()
jabberSignOn _ = undefined

ircSignOn :: String -> String -> LB ()
ircSignOn nick ircname = do
    server <- asks ircServer

    -- password support. TODO: Move this to IRC?
    -- If plugin initialising was delayed till after we connected, we'd
    -- be able to write a Passwd plugin.
    send . Just $ IRC.user nick server ircname
    send . Just $ IRC.setNick nick
    mpasswd <- liftIO (handleJust ioErrors (const (return "")) $
                       readFile "State/passwd")
    case readM mpasswd of
      Nothing     -> return ()
      Just passwd -> ircPrivmsg "nickserv" $ Just $ "identify " ++ passwd

ircGetChannels :: LB [String]
ircGetChannels = (map getCN . M.keys) `fmap` gets ircChannels

-- Send a quit message, settle and wait for the server to drop our
-- handle. At which point the main thread gets a closed handle eof
-- exceptoin, we clean up and go home
ircQuit :: String -> LB ()
ircQuit msg = do
    modify $ \state -> state { ircStayConnected = False }
    send  . Just $ IRC.quit msg
    liftIO $ threadDelay 1000
    io $ hPutStrLn stderr "Quit"

ircReconnect :: String -> LB ()
ircReconnect msg = do
    send . Just $ IRC.quit msg
    liftIO $ threadDelay 1000

ircRead :: LB (Maybe IRC.IrcMessage)
ircRead = do
    chanr <- asks ircReadChan
    liftIO (readChan chanr)

-- 
-- convenient wrapper
--
send_ :: IRC.IrcMessage -> LB ()
send_ = send . Just

send :: Maybe IRC.IrcMessage -> LB ()
send line = do
    chanw <- asks ircWriteChan
    io (writeChan chanw line)

-- | Send a message to a channel\/user. If the message is too long, the rest
--   of it is saved in the (global) more-state.
ircPrivmsg :: String        -- ^ The channel\/user.
           -> Maybe String  -- ^ The message.
           -> LB ()

ircPrivmsg who Nothing = ircPrivmsg' who Nothing
ircPrivmsg who (Just msg) = do
    filters   <- gets ircOutputFilters
    sendlines <- foldr (\f -> (=<<) (f who)) ((return . lines) msg) $ map snd filters
    mapM_ (\s -> ircPrivmsg' who (Just $ take textwidth s)) (take 10 sendlines)

-- A raw send version
ircPrivmsg' :: String -> Maybe String -> LB ()
ircPrivmsg' who (Just "")  = ircPrivmsg' who (Just " ")
ircPrivmsg' who (Just msg) = send . Just $ IRC.privmsg who msg
ircPrivmsg' _   Nothing    = send Nothing

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
           -> (forall mod s. Module mod s => mod -> ModuleT s LB a)
           -> LB a

withModule dict modname def f = do
    maybemod <- gets (M.lookup modname . dict)
    case maybemod of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (ModuleRef m ref name) -> let ?ref = ref; ?name = name in f m
      _                           -> def

-- | Interpret a function in the context of all modules
withAllModules :: (forall mod s. Module mod s => mod -> ModuleT s LB a) -> LB [a]
withAllModules f = do
    mods <- gets $ M.elems . ircModules :: LB [ModuleRef]
    (`mapM` mods) $ \(ModuleRef m ref name) -> do
        let ?ref = ref; ?name = name
        f m

getDictKeys :: (MonadState s m) => (s -> Map k a) -> m [k]
getDictKeys dict = gets (M.keys . dict)

------------------------------------------------------------------------

-- convenience:
io :: forall a (m :: * -> *). (MonadIO m) => IO a -> m a
io = liftIO
{-# INLINE io #-}

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

-- | wrap long lines.
lineify :: OutputFilter
lineify = const (return . mlines . unlines)

-- | Don't send any output to alleged bots.
checkRecip :: OutputFilter
checkRecip who msg
    | who == Config.name Config.config       = return []
    | "bot" `isSuffixOf` lowerCaseString who = return []
    | otherwise                              = return msg

-- | For now, this just checks for duplicate empty lines.
cleanOutput :: OutputFilter
cleanOutput _ msg = return $ remDups True msg'
    where
        remDups True  ("":xs) =    remDups True xs
        remDups False ("":xs) = "":remDups True xs
        remDups _     (x: xs) = x: remDups False xs
        remDups _     []      = []
        msg' = map dropSpace msg

-- | Divide the lines' indent by three.
reduceIndent :: OutputFilter
reduceIndent _ msg = return $ map redLine msg
    where
        redLine (' ':' ':' ':xs) = ' ': redLine xs
        redLine (' ':' ':xs)     = ' ': redLine xs
        redLine xs               = xs

-- ---------------------------------------------------------------------
-- | Output filter
--
mlines :: String -> [String]
mlines = (mbreak =<<) . lines
    where
        mbreak :: String -> [String]
        mbreak xs
            | null bs   = [as]
            | otherwise = (as++cs) : filter (not . null) (mbreak (dropWhile isSpace ds))
            where
                (as,bs) = splitAt (w-n) xs
                breaks  = filter (not . isAlphaNum . last . fst) $ drop 1 $
                                  take n $ zip (inits bs) (tails bs)
                (cs,ds) = last $ (take n bs, drop n bs): breaks
                w = textwidth
                n = 10
