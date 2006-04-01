{-# OPTIONS -cpp #-}
--
-- | Lambdabot monad and parts of the IRC protocol binding
--
module Lambdabot (
        MODULE(..), Module(..),
        ModuleT, ModState, ModuleLB, Mode(..),

        IRC.Message(..),
        IRCRState(..), IRCRWState(..), IRCError(..),

        LB, mapLB, lbIO,

        withModule, getDictKeys,

        send,
        ircPrivmsg, ircPrivmsg', -- not generally used
        ircQuit, ircReconnect,
        ircGetChannels,
        ircSignalConnect, Callback, ircInstallOutputFilter, OutputFilter,
        ircInstallModule, ircUnloadModule,
        ircSignOn,
        ircRead,

        ircLoad, ircUnload,

        checkPrivs, mkCN, handleIrc, runIrc,

        io,
  ) where

import qualified Config (config, name, admins, host, port)
import ErrorUtils       (bracketError, tryErrorJust, finallyError, catchErrorJust, tryError)
import Util             (lowerCaseString)
import Serial
import qualified IRC

import qualified Data.FastPackedString as P

import Map (Map)
import qualified Map as M hiding (Map)
import qualified Shared as S

import Prelude hiding   (mod, catch)

import Network          (withSocketsDo, connectTo, PortID(PortNumber))

import System.IO        (Handle, hGetLine, hPutStr, hClose, stdin, stdout,
                         hSetBuffering, BufferMode(NoBuffering))

import System.IO.Error  (isEOFError, ioeGetHandle)

# ifndef mingw32_HOST_OS
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

import System.Console.Readline  (readline, addHistory)

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
        ircReadChan    :: Chan IRC.Message,
        ircReadThread  :: ThreadId,
        ircWriteChan   :: Chan IRC.Message,
        ircWriteThread :: ThreadId
  }

type Callback = IRC.Message -> LB ()

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

-- Man, I hate dynamics
newtype SignalException = SignalException Signal deriving Typeable

------------------------------------------------------------------------

withHandler :: (MonadIO m,MonadError e m) => Signal -> Handler -> m () -> m ()

#ifdef mingw32_HOST_OS
withHandler s h m = return ()
#else
withHandler s h m
  = bracketError (io $ installHandler s h Nothing)
                 (\oldh -> io $ installHandler s oldh Nothing)
                 (const m)
#endif

withHandlerList :: (MonadError e m,MonadIO m)
                => [Signal]
                -> (Signal -> Handler)
                -> m ()
                -> m ()
withHandlerList sl h m = foldr (\s -> withHandler s (h s)) m sl

-- be careful adding signals, some signals can't be caught and installHandler
-- just raises an exception if you try
ircSignalsToCatch :: [Signal]
ircSignalsToCatch = [
#ifndef mingw32_HOST_OS
                     busError,
                     segmentationViolation,
                     keyboardSignal,softwareTermination,
                     keyboardTermination,lostConnection
#endif
                     ]

ircSignalMessage :: Signal -> [Char]
ircSignalMessage s
#ifndef mingw32_HOST_OS
   | s==busError                = "killed by SIGBUS"
   | s==segmentationViolation   = "killed by SIGSEGV"
   | s==keyboardSignal          = "killed by SIGINT"
   | s==softwareTermination     = "killed by SIGTERM"
   | s==keyboardTermination     = "killed by SIGQUIT"
   | s==lostConnection          = "killed by SIGHUP"
#endif
 -- this case shouldn't happen if the list of messages is kept up to date
 -- with the list of signals caught
   | otherwise                  = "killed by unknown signal"

ircSignalHandler :: ThreadId -> Signal -> Handler
ircSignalHandler threadid s
#ifdef mingw32_HOST_OS
  = ()
#else
  = Catch $ do putMVar catchLock ()
               throwDynTo threadid $ SignalException s

-- This is clearly a hack, but I have no idea how to accomplish the same
-- thing correctly. The main problem is that signals are often thrown multiple
-- times, and the threads start killing each other if we allow the
-- SignalException to be thrown more than once.
{-# NOINLINE catchLock #-}
catchLock :: MVar ()
catchLock = unsafePerformIO newEmptyMVar
#endif

withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m = do
    io $ installHandler sigPIPE Ignore Nothing
    io $ installHandler sigALRM Ignore Nothing
    threadid <- io myThreadId
    withHandlerList ircSignalsToCatch (ircSignalHandler threadid) m

-- "Phantom Error". Maybe we should handle both errors separately?
data IRCError = IRCRaised Exception | SignalCaught Signal deriving Show

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
newtype LB a = LB { runLB :: ReaderT (IORef (Maybe IRCRState),IORef IRCRWState) IO a }

#ifndef __HADDOCK__
  deriving (Monad,Functor,MonadIO)
#endif

mapLB :: (IO a -> IO b) -> LB a -> LB b
mapLB f = LB . mapReaderT f . runLB -- lbIO (\conv -> f (conv lb))

-- lbIO return :: LB (LB a -> IO a)
-- CPS to work around predicativiy of haskell's type system.
lbIO :: ((forall a. LB a -> IO a) -> IO b) -> LB b
lbIO k = LB $ ReaderT $ \r -> k (\(LB m) -> m `runReaderT` r)

-- All of IRCErrorT's (RIP) functionality can be shrunk down to that.
instance MonadError IRCError LB where
  throwError (IRCRaised e) = io $ throwIO e
  throwError (SignalCaught e) = io $ evaluate (throwDyn $ SignalException e)
  m `catchError` h = lbIO $ \conv -> (conv m
              `catchDyn` \(SignalException e) -> conv $ h $ SignalCaught e)
              `catch` \e -> conv $ h $ IRCRaised e


-- Actually, this isn't a reader anymore
instance MonadReader IRCRState LB where
  ask = LB $ fmap (maybe (error "No connection") id) $
    io . readIORef =<< asks fst
  local = error "You are not supposed to call local"

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

evalLB :: LB a -> IRCRWState -> IO a
evalLB (LB lb) rws = do
    ref  <- newIORef rws
    ref' <- newIORef Nothing
    lb `runReaderT` (ref',ref)

-- | This \"transformer\" encodes the additional information a module might 
--   need to access its name or its state.
type ModuleT s m a = (?ref :: MVar s, ?name :: String) => m a

-- | A nicer synonym for some ModuleT stuffs
type ModuleLB m = ModuleT m LB [String]

-- Name !!!
type ModState s a = (?ref :: MVar s, ?name :: String) => a

ircSignOn :: String -> String -> LB ()
ircSignOn nick ircname = do
    server <- asks ircServer

    -- password support. TODO: Move this to IRC?
    send $ IRC.mkMessage "USER" [nick, "localhost", server, ircname]
    send $ IRC.mkMessage "NICK" [nick]
    mpasswd <- liftIO (handleJust ioErrors (const (return "")) $
                       readFile "State/passwd")
    case readM mpasswd of
      Nothing     -> return ()
      Just passwd -> ircPrivmsg "nickserv" $ "identify " ++ passwd

ircGetChannels :: LB [String]
ircGetChannels = do
    chans <- gets ircChannels
    return $ map getCN (M.keys chans)

-- quit and reconnect wait 1s after sending a QUIT to give the connection
-- a chance to close gracefully (and interrupt the wait with an exception)
-- after that they return and the connection will probably be forcibly
-- closed by the finallyError in runIrc'

ircQuit :: String -> LB ()
ircQuit msg = do
    modify $ \state -> state { ircStayConnected = False }
    send   $ IRC.quit msg
    liftIO $ threadDelay 1000

ircReconnect :: String -> LB ()
ircReconnect msg = do
    send $ IRC.quit msg
    liftIO $ threadDelay 1000

ircRead :: LB IRC.Message
ircRead = do
    chanr <- asks ircReadChan
    liftIO (readChan chanr)

send :: IRC.Message -> LB ()
send (IRC.Message x y z) | x `seq` y `seq` z `seq` False = undefined -- strictify
send line = do
    chanw <- asks ircWriteChan
    io (writeChan chanw $! line)

----------------------------------------------------------------------

textwidth :: Int
textwidth = 350 -- IRC maximum msg length, minus a bit for safety.

lineify, checkRecip, cleanOutput, reduceIndent :: OutputFilter
-- | wrap long lines.
lineify = const (return . mlines . unlines)

-- | Don't send any output to alleged bots.
checkRecip who msg
  | who == Config.name Config.config       = return []
  | "bot" `isSuffixOf` lowerCaseString who = return []
  | otherwise                              = return msg

-- | For now, this just checks for duplicate empty lines.
cleanOutput _ msg = return $ remDups True msg'
    where
        remDups True  ("":xs) =    remDups True xs
        remDups False ("":xs) = "":remDups True xs
        remDups _     (x: xs) = x: remDups False xs
        remDups _     []      = []
        msg' = map (reverse . dropWhile isSpace . reverse) msg

-- | Divide the lines' indent by three.
reduceIndent _ msg = return $ map redLine msg 
    where
        redLine (' ':' ':' ':xs) = ' ': redLine xs
        redLine (' ':' ':xs)     = ' ': redLine xs
        redLine xs               = xs

-- ---------------------------------------------------------------------

-- | Send a message to a channel\/user. If the message is too long, the rest
--   of it is saved in the (global) more-state.
ircPrivmsg :: String -- ^ The channel\/user.
           -> String -- ^ The message.
           -> LB ()

ircPrivmsg who msg = do
    filters   <- gets ircOutputFilters
    sendlines <- foldr (\f -> (=<<) (f who)) ((return . lines) msg) $ map snd filters
    mapM_ (ircPrivmsg' who . take textwidth) $ take 10 sendlines

-- A raw send version
ircPrivmsg' :: String -> String -> LB ()
ircPrivmsg' who "" = ircPrivmsg' who " "
ircPrivmsg' who msg = send $ IRC.privmsg who msg

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

-- ---------------------------------------------------------------------

--
-- May wish to add more things to the things caught, or restructure things 
-- a bit. Can't just catch everything - in particular EOFs from the socket
-- loops get thrown to this thread and we musn't just ignore them.
--
handleIrc :: (MonadError IRCError m) => (String -> m ()) -> m () -> m ()
handleIrc handler m = catchError m $ \e -> case e of
        IRCRaised s -> handler $ show s
        _           -> throwError e

------------------------------------------------------------------------
--
-- Lambdabot modes, networked , or command line
--
data Mode = Online | Offline deriving Eq

--
-- | run the IRC monad
--
runIrc :: Mode -> LB a -> LB () -> S.DynLoad -> IO ()
runIrc mode initialise loop ld = withSocketsDo $ do
    try $ evalLB
             (initialise >> withIrcSignalCatch (runIrc' mode loop))
             (initState (Config.admins Config.config) ld)
    return ()

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
runIrc' :: Mode -> LB a -> LB ()
runIrc' mode loop = do

    (hin,hout,rloop,wloop) <- case mode of
        Online  -> do
            let portnum  = PortNumber $ fromIntegral (Config.port Config.config)
            s <- io $ connectTo (Config.host Config.config) portnum
            return (s,s,readerLoop,writerLoop)
        Offline -> return (stdin, stdout, offlineReaderLoop, offlineWriterLoop)

    tryErrorJust (isEOFon hin) $ do
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
                        ircWriteThread = threadw }

        finallyError
           (localLB (Just chans) $ catchSignals $ loop >> ircQuit "terminated")
           (io $ do killThread threadr
                    killThread threadw
                    when (mode == Online) $ hClose hin)

    reconn <- gets ircStayConnected
    if reconn then runIrc' mode loop else exitModules

  where
        isEOFon s (IRCRaised (IOException e))
            = if isEOFError e && ioeGetHandle e == Just s then Just () else Nothing
        isEOFon _ _ = Nothing
        isSignal (SignalCaught s) = Just s
        isSignal _ = Nothing

        -- catches a signal, quit with message
        catchSignals n = catchErrorJust isSignal n $ \s -> do
             tryError $ ircQuit (ircSignalMessage s)
             return ()

        exitModules = do
          mods <- gets $ M.elems . ircModules
          (`mapM_` mods) $ \(ModuleRef mod ref name) -> do
            -- Call ircUnloadModule?
            let ?ref = ref; ?name = name
            moduleExit mod
            writeGlobalState mod name

------------------------------------------------------------------------
--
-- online reader loop, the mvars are unused
--
readerLoop :: ThreadId -> Chan IRC.Message -> Chan IRC.Message -> Handle
           -> MVar () -> MVar () -> IO ()
readerLoop threadmain chanr chanw h _ _ = do
    io (putStrLn "Running reader loop...")
    exc <- try readerLoop'
    case exc of
        Left (AsyncException ThreadKilled) -> return ()
        Left err                           -> throwTo threadmain err
        Right _                            -> return ()
  where
    readerLoop' = do
        line <- hGetLine h
        let line' = [ c | c <- line, c /= '\n', c /= '\r' ]
        case line' of
            ('P':'I':'N':'G':' ':rest) -> writeChan chanw (IRC.mkMessage "PONG" [rest])
            _                          -> writeChan chanr (IRC.decodeMessage line')
        readerLoop'
{-# INLINE readerLoop #-}

--
-- online writer loop
--
-- flood control: RFC 2813, section 5.8
--
writerLoop :: ThreadId -> Chan IRC.Message -> Handle -> MVar () -> MVar () -> IO ()
writerLoop threadmain chanw h _ _ = do
    sem1 <- newQSem 0
    sem2 <- newQSem 5
    forkIO $ sequence_ . repeat $ do
           waitQSem sem1
           threadDelay 2000000
           signalQSem sem2
    exc <- try $ writerLoop' (sem1,sem2)
    case exc of
           Left (AsyncException ThreadKilled)
             -> try (hPutStr h "QUIT : died unexpectedly\r") >> return ()
           Left e  -> throwTo threadmain e
           Right _ -> return ()

  where
    writerLoop' sems@(sem1,sem2) = do
           msg <- readChan chanw
           waitQSem sem2
           hPutStr h $ IRC.encodeMessage msg "\r"
           signalQSem sem1
           writerLoop' sems
{-# INLINE writerLoop #-}

------------------------------------------------------------------------

-- 
-- Offline reader and writer loops. A prompt with line editing
-- Takes a string from stdin, wraps it as an irc message, and _blocks_
-- waiting for the writer thread (to keep things in sync).
--
-- We (incorrectly) assume there's at least one write for every read.
-- If a command returns no output (i.e. @more on an empty buffer) then
-- we block in offline mode :(
-- 
-- the mvars are used to keep the normally async threads in step.
--
offlineReaderLoop :: ThreadId -> Chan IRC.Message -> Chan IRC.Message -> Handle
                  -> MVar () -> MVar () -> IO ()
offlineReaderLoop threadmain chanr _chanw _h syncR syncW = do
    exc <- try readerLoop'
    case exc of
        Right _                            -> return ()
        Left (AsyncException ThreadKilled) -> error "quit"
        Left e                             -> throwTo threadmain e
  where
    readerLoop' = do

        takeMVar syncR  -- wait till writer lets us proceed

        s <- readline "lambdabot> " -- read stdin
        case s of
            Nothing -> error "EOF"
            Just x -> let s' = dropWhile isSpace x
                      in if null s' then putMVar syncR () >> readerLoop' else do
                addHistory s'

                let msg = case s' of
                            "quit" -> error "<quit>"
                            '>':xs -> "@eval " ++ xs
                            _      -> "@"      ++ dropWhile (== ' ') s'

                msg `seq` return () -- force error, perhaps. I know I'm bad

                let m  = IRC.Message { IRC.msgPrefix  = "dons!n=user@null"
                                     , IRC.msgCommand = "PRIVMSG"
                                     , IRC.msgParams  = ["#haskell",":" ++ msg ] }
                writeChan chanr m
                putMVar syncW () -- let writer go 
                readerLoop'

--
-- Print to stdout
--
offlineWriterLoop :: ThreadId -> Chan IRC.Message -> Handle
                  -> MVar () -> MVar () -> IO ()
offlineWriterLoop threadmain chanw h syncR syncW = do
    exc <- try writerLoop'
    case exc of
        Right _                            -> return ()
        Left (AsyncException ThreadKilled) -> return () -- silently quit
        Left e                             -> throwTo threadmain e
  where
    writerLoop' = do

        takeMVar syncW -- wait for reader to let us go

        let loop = do
            msg <- readChan chanw   -- eventually 'send' puts a message on this chan
            let str = case (tail . IRC.msgParams) msg of
                        []    -> []
                        (x:_) -> tail x
            hPutStr h (str ++ "\n")     -- write stdout
            b <- isEmptyChan chanw
            when (not b) loop
        loop

        putMVar syncR () -- now allow writer to go
        writerLoop'

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
    process         :: m                    -- ^ phantom     (required)
        -> IRC.Message                      -- ^ the message (uneeded by most?)
        -> String                           -- ^ target      (not needed)
        -> String                           -- ^ command
        -> String                           -- ^ the arguments to the command
        -> ModuleLB s                       -- ^ maybe output

    -- | Like process, but uncommonly used args are ignored
    -- Lambdabot will attempt to run process first, and then fall back
    -- to process_, which in turn has a default instance.
    --
    process_ :: m                           -- ^ phantom
        ->  String -> String                -- ^ command, args
        -> ModuleLB s                       -- ^ maybe output

------------------------------------------------------------------------

    process_ _ _ _     = return []
    moduleHelp m _     = concat (map ('@':) (moduleCmds m))
    modulePrivs _      = []
    moduleCmds      _  = []
    moduleExit _       = return ()
    moduleInit _       = return ()
    moduleSticky _     = False
    moduleSerialize _  = Nothing
    moduleDefState  _  = return $ error "state not initalized"

------------------------------------------------------------------------

-- | An existential type holding a module, used to represent modules on
-- the value level, for manipluation at runtime by the dynamic linker.
--
data MODULE = forall m s. (Module m s) => MODULE m

data ModuleRef = forall m s. (Module m s) => ModuleRef m (MVar s) String

------------------------------------------------------------------------

toFilename :: String -> String
toFilename = ("State/"++)

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

readFile' :: String -> IO P.FastString
readFile' = P.mmapFile
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
    let ?ref = ref; ?name = modname -- yikes
    moduleInit mod
    let cmds  = moduleCmds mod
        privs = modulePrivs mod

    s <- get
    let modmap = ircModules s
        cmdmap = ircCommands s
    put $ s {
      ircModules = M.insert modname modref modmap,
      ircCommands = M.addList [ (cmd,modref) | cmd <- cmds++privs ] cmdmap,
      ircPrivCommands = ircPrivCommands s ++ privs
    }

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
              Nothing -> put (s { ircCallbacks = M.insert str [(?name,f)]    cbs}) 
              Just fs -> put (s { ircCallbacks = M.insert str ((?name,f):fs) cbs}) 

ircInstallOutputFilter :: OutputFilter -> ModuleT s LB ()
ircInstallOutputFilter f = modify $ \s -> 
  s { ircOutputFilters = (?name, f): ircOutputFilters s }

-- | Checks if the given user has admin permissions and excecute the action
--   only in this case.
checkPrivs :: IRC.Message -> LB Bool
checkPrivs msg = gets (isJust . M.lookup (IRC.nick msg) . ircPrivilegedUsers)

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
      Just (ModuleRef m ref name) -> let ?ref = ref; ?name = name in f m
      _                           -> def


getDictKeys :: (MonadState s m) => (s -> Map k a) -> m [k]
getDictKeys dict = gets (M.keys . dict)

------------------------------------------------------------------------

-- convenience:
io :: forall a (m :: * -> *). (MonadIO m) => IO a -> m a
io = liftIO
{-# INLINE io #-}
