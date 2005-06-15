--
-- | Lambdabot monad and IRC protocol binding
-- TODO : refactor this. Especially the MODULE stuff which has nothing
-- to do with the IRC protocol.
--
module Lambdabot (
        MODULE(..), Module(..),
        ModuleT, ModState,

        IRCMessage(..), 
        IRCRState(..), IRCRWState(..), IRCError(..), 
        IRC,

        LB, mapLB, lbIO,

        withModule, getDictKeys,

        ircPrivmsg, ircPrivmsg',
        ircJoin, ircPart, ircQuit, ircReconnect,
        ircTopic, ircGetTopic, ircGetChannels,
        ircSignalConnect, Callback, ircInstallOutputFilter, OutputFilter,
        ircInstallModule, ircUnloadModule,
        ircNick, ircChans, ircNames,
        ircSignOn,
        ircRead,

        ircLoad, ircUnload,

        clean, checkPrivs, mkCN, handleIrc, runIrc,
  ) where

import qualified Config (config, name, admins, host, port, textwidth)
import DeepSeq          (($!!), DeepSeq(..))
import ErrorUtils       (bracketError, tryErrorJust, finallyError, catchErrorJust, tryError)
import Util             (split,clean,breakOnGlue, Serializer(..), lowerCaseString, readM)
import qualified Util   (join)

import Map (Map)
import qualified Map as M hiding (Map)
import qualified Shared as S

import Prelude hiding   (mod, catch)

import Network          (withSocketsDo, connectTo, PortID(PortNumber))

import System.IO        (Handle, hGetLine, hPutStr, hClose,
                         hSetBuffering, BufferMode(NoBuffering),
                         openFile, hGetContents, IOMode(..))

#if __GLASGOW_HASKELL__ >= 600
import System.IO.Error  (isEOFError, ioeGetHandle)

# ifndef mingw32_HOST_OS
import System.Posix.Signals
import System.IO.Unsafe         (unsafePerformIO)
# endif

#else
import Posix
import System.IO.Error
#endif

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
        ircReadChan    :: Chan IRCMessage,
        ircReadThread  :: ThreadId,
        ircWriteChan   :: Chan IRCMessage,
        ircWriteThread :: ThreadId
  }

{-
data Connection = Connection {
  server    :: String,
  readChan  :: Chan RMessage,
  writeChan :: Chan (WMessage, IO ()),
  thread    :: MVar (Maybe ThreadID),
  handle    :: Handle
}
-}

type Callback = IRCMessage -> LB ()
-- | target, message
type OutputFilter = String -> [String] -> IRC [String]
-- type OutputFilter = String -> [String] -> ContT [String] IRC [String]

-- | Global read\/write state.
data IRCRWState
  = IRCRWState {
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

-- only use the "smart constructor":
mkCN :: String -> ChanName
mkCN = ChanName . map toLower

-- Man, I hate dynamics
newtype SignalException = SignalException Signal
  deriving Typeable

withHandler :: (MonadIO m,MonadError e m)
            => Signal 
            -> Handler 
            -> Maybe SignalSet 
            -> m () 
            -> m ()
#ifdef mingw32_HOST_OS
withHandler s h ss m = return ()
#else
withHandler s h ss m 
  = bracketError (liftIO $ installHandler s h ss)
                 (\oldh -> liftIO $ installHandler s oldh Nothing)
                 (\_ -> m)
#endif

withHandlerList :: (MonadError e m,MonadIO m)
                => [Signal]
                -> (Signal -> Handler) 
                -> Maybe SignalSet 
                -> m () 
                -> m ()
withHandlerList sl h ss m = foldr (\s -> withHandler s (h s) ss) m sl

-- be careful adding signals, some signals can't be caught and installHandler
-- just raises an exception if you try
ircSignalsToCatch :: [Signal]
ircSignalsToCatch = [
#ifndef mingw32_HOST_OS
#if __GLASGOW_HASKELL__ >= 600
                     busError,
#endif
                     segmentationViolation,
                     keyboardSignal,softwareTermination,
                     keyboardTermination,lostConnection
#endif                     
                     ]

ircSignalMessage :: Signal -> [Char]
ircSignalMessage s
#ifndef mingw32_HOST_OS
#if __GLASGOW_HASKELL__ >= 600
                   | s==busError = "killed by SIGBUS"
#endif
                   | s==segmentationViolation = "killed by SIGSEGV"
                   | s==keyboardSignal = "killed by SIGINT"
                   | s==softwareTermination = "killed by SIGTERM"
                   | s==keyboardTermination = "killed by SIGQUIT"
                   | s==lostConnection = "killed by SIGHUP"
#endif
 -- this case shouldn't happen if the list of messages is kept up to date
 -- with the list of signals caught
                   | otherwise = "killed by unknown signal"

ircSignalHandler :: ThreadId -> Signal -> Handler
ircSignalHandler threadid s
#ifdef mingw32_HOST_OS
  = ()
#else  
  = Catch $ do
      putMVar catchLock ()
      throwDynTo threadid $ SignalException s
                                
-- This is clearly a hack, but I have no idea how to accomplish the same
-- thing correctly. The main problem is that signals are often thrown multiple
-- times, and the threads start killing each other if we allow the
-- SignalException to be thrown more than once.
{-# NOINLINE catchLock #-}      
catchLock :: MVar ()            
catchLock = unsafePerformIO $ newEmptyMVar
#endif  

withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m 
  = do threadid <- liftIO $ myThreadId
       withHandlerList ircSignalsToCatch (ircSignalHandler threadid) Nothing m

-- "Phantom Error". Maybe we should handle both errors separately?
data IRCError = IRCRaised Exception 
              | SignalCaught Signal 
  deriving Show

{-
ircErrorMsg :: IRCError -> String
ircErrorMsg (IRCRaised e) = show e
ircErrorMsg (SignalCaught s) = "caught signal "++show s
-}


data IRCMessage
  = IRCMessage {
        msgPrefix   :: String,
        msgCommand  :: String,
        msgParams   :: [String]
  }
  deriving (Show)

instance DeepSeq IRCMessage where
  deepSeq m
    = deepSeq (msgPrefix m) . deepSeq (msgCommand m) . deepSeq (msgParams m)

ircNick     :: IRCMessage -> String
ircNick msg = fst $ breakOnGlue "!" (msgPrefix msg)

-- | 'ircChans' converts an IRCMessage to a list of channels.
ircChans :: IRCMessage -> [String]
ircChans msg
  = let cstr = head $ msgParams msg
    in map (\(x:xs) -> if x == ':' then xs else x:xs) (split "," cstr)
           -- solves what seems to be an inconsistency in the parser

{-
ircuser     :: IRCMessage -> String
ircuser msg = head $ split "@" $ (split "!" (msgPrefix msg)) !! 1

irchost     :: IRCMessage -> String
irchost msg = (split "@" (msgPrefix msg)) !! 1
-}

type IRC = LB

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
newtype LB a 
  = LB { runLB :: ReaderT (IORef (Maybe IRCRState), IORef IRCRWState) IO a }
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
  throwError (IRCRaised e) = liftIO $ throwIO e
  throwError (SignalCaught e) = liftIO $ evaluate (throwDyn $ SignalException e)
  m `catchError` h = lbIO $ \conv -> (conv m
              `catchDyn` \(SignalException e) -> conv $ h $ SignalCaught e)
              `catch` \e -> conv $ h $ IRCRaised e


-- Actually, this isn't a reader anymore
instance MonadReader IRCRState LB where
  ask = LB $ fmap (maybe (error "No connection") id) $
    liftIO . readIORef =<< asks fst
  local = error "You are not supposed to call local"

localLB :: Maybe IRCRState -> LB a -> LB a
localLB new (LB m) = LB $ do
  ref <- asks fst
  old <- liftIO $ readIORef ref
  liftIO $ writeIORef ref new
  res <- m
  liftIO $ writeIORef ref old
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
  ref <- newIORef rws
  ref' <- newIORef Nothing
  lb `runReaderT` (ref',ref)

-- | This \"transformer\" encodes the additional information a module might 
--   need to access its name or its state.
type ModuleT s m a = (?ref :: MVar s, ?name :: String) => m a

-- Name !!!
type ModState s a = (?ref :: MVar s, ?name :: String) => a

mkIrcMessage :: String -> [String] -> IRCMessage
mkIrcMessage cmd params
  = IRCMessage { msgPrefix = "", msgCommand = cmd, msgParams = params }

ircSignOn :: String -> String -> IRC ()
ircSignOn nick ircname = do 
    server <- asks ircServer
    ircWrite (mkIrcMessage "USER" [nick, "localhost", server, ircname])
    ircWrite (mkIrcMessage "NICK" [nick])
    mpasswd <- liftIO (handleJust ioErrors (const (return "")) $
                       readFile "State/passwd")
    case readM mpasswd of
      Nothing     -> return ()
      Just passwd -> ircPrivmsg "nickserv" $ "identify " ++ passwd

ircGetChannels :: LB [String]
ircGetChannels = do 
    chans <- gets ircChannels
    return $ map getCN (M.keys chans)

lineify, checkRecip, cleanOutput, reduceIndent :: OutputFilter
-- | wrap long lines.
lineify _ msg = return $ mlines $ unlines msg

-- | Don't send any output to alleged bots.
checkRecip who msg
  | who == Config.name Config.config = return []
  | "bot" `isSuffixOf` lowerCaseString who = return []
  | otherwise = return msg

-- | For now, this just checks for duplicate empty lines.
cleanOutput _ msg = return $ remDups True msg' where
  remDups True  ("":xs) =    remDups True xs
  remDups False ("":xs) = "":remDups True xs
  remDups _     (x: xs) = x: remDups False xs
  remDups _     []      = []
  msg' = map (reverse . dropWhile isSpace . reverse) msg

-- | Divide the lines' indent by three.
reduceIndent _ msg = return $ map redLine msg where
  redLine (' ':' ':' ':xs) = ' ': redLine xs
  redLine (' ':' ':xs)     = ' ': redLine xs
  redLine xs               = xs

-- | Send a message to a channel\/user. If the message is too long, the rest
--   of it is saved in the (global) more-state.
ircPrivmsg :: String -- ^ The channel\/user.
   -> String         -- ^ The message.
   -> IRC ()
ircPrivmsg who msg = do 
  filters <- gets ircOutputFilters
  sendlines <- foldr (\f -> (=<<) (f who)) (return $ lines msg) $ map snd filters
  -- Hardcoded defaults: maximal ten lines, maximal 100 chars/line
  mapM_ (ircPrivmsg' who . take 100) $ take 10 sendlines

-- ---------------------------------------------------------------------
-- | output filter (should consider a fmt(1)-like algorithm
--
mlines          :: String -> [String]
mlines s        = mbreak =<< lines s where
  mbreak :: String -> [String]
  mbreak xs
    | null bs = [as]
    | otherwise = (as++cs):filter (not . null) (mbreak (dropWhile isSpace ds))
    where 
    (as,bs) = splitAt (w-n) xs
    breaks  = filter (not . isAlphaNum . last . fst) $ drop 1 $
      take n $ zip (inits bs) (tails bs)
    (cs,ds) = last $ (take n bs, drop n bs): breaks
    w = Config.textwidth Config.config
    n = 10

-- ---------------------------------------------------------------------

ircPrivmsg' :: String -> String -> IRC ()
ircPrivmsg' who "" = ircPrivmsg' who " "
ircPrivmsg' who msg
  = ircWrite (mkIrcMessage "PRIVMSG" [who, ':' : clean_msg])
    -- merry christmas det
    where clean_msg = case concatMap clean msg of
              str@('@':_) -> ' ':str
              str         -> str


ircTopic :: String -> String -> IRC ()
ircTopic chan topic
  = ircWrite (mkIrcMessage "TOPIC" [chan, ':' : topic])

ircGetTopic :: String -> IRC ()
ircGetTopic chan
  = ircWrite (mkIrcMessage "TOPIC" [chan])

-- quit and reconnect wait 1s after sending a QUIT to give the connection
-- a chance to close gracefully (and interrupt the wait with an exception)
-- after that they return and the connection will probably be forcibly
-- closed by the finallyError in runIrc'

ircQuit :: String -> IRC ()
ircQuit msg = do 
    state <- get
    put (state { ircStayConnected = False })
    ircWrite (mkIrcMessage "QUIT" [':' : msg])
    liftIO $ threadDelay 1000

ircReconnect :: String -> IRC ()
ircReconnect msg = do ircWrite (mkIrcMessage "QUIT" [':' : msg])
                      liftIO $ threadDelay 1000

ircJoin :: String -> IRC ()
ircJoin loc
  = ircWrite (mkIrcMessage "JOIN" [loc])

ircPart :: String -> IRC ()
ircPart loc
  = ircWrite (mkIrcMessage "PART" [loc])

ircNames :: [String] -> IRC ()
ircNames chans = ircWrite (mkIrcMessage "NAMES" [Util.join "," chans])

ircRead :: IRC IRCMessage
ircRead = do 
    chanr <- asks ircReadChan
    liftIO (readChan chanr)

ircWrite :: IRCMessage -> IRC ()
ircWrite line = do  
    chanw <- asks ircWriteChan
    -- use DeepSeq's $!! to ensure that any Haskell errors in line
    -- are caught now, rather than later on in the other thread
    liftIO (writeChan chanw $!! line)


--
-- May wish to add more things to the things caught, or restructure things 
-- a bit. Can't just catch everything - in particular EOFs from the socket
-- loops get thrown to this thread and we musn't just ignore them.
--
handleIrc :: (MonadError IRCError m) => (String -> m ()) -> m () -> m ()
handleIrc handler m = catchError m $ \e -> case e of
        IRCRaised s -> handler $ show s
        _           -> throwError e

--
-- | run the IRC monad
--
runIrc :: LB a -> IRC () -> S.DynLoad -> IO ()
runIrc initialise m ld = withSocketsDo $ do
        ex <- try $ evalLB
                 (initialise >> withIrcSignalCatch (runIrc' m))
                 (initState (Config.admins Config.config))
        either (\e->putStrLn ("runIRC: caught exception: "++show e)) (return) ex
      where
        initState as = IRCRWState {
                ircPrivilegedUsers = M.fromList [(a,True)|a <- as],
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

{-
traceError :: (MonadIO m,MonadError e m,Show e) => m a -> m a
traceError = handleError (\e -> liftIO (print e) >> throwError e)

traceException :: (MonadIO m,MonadException m) => m a -> m a
traceException = handleM (\e -> liftIO (print e) >> throwM e)
-}

runIrc' :: IRC () -> LB ()
runIrc' m = do  
        let portnum  = PortNumber $ fromIntegral (Config.port Config.config)

        s <- liftIO $ connectTo (Config.host Config.config) portnum

        tryErrorJust (isEOFon s) $ do 
            liftIO $ hSetBuffering s NoBuffering
            threadmain <- liftIO $ myThreadId
            chanr      <- liftIO $ newChan
            chanw      <- liftIO $ newChan
            threadr    <- liftIO $ forkIO $ readerLoop threadmain chanr chanw s
            threadw    <- liftIO $ forkIO $ writerLoop threadmain chanw s

            let chans = IRCRState {
                            ircServer      = Config.host Config.config,
                            ircReadChan    = chanr,
                            ircReadThread  = threadr,
                            ircWriteChan   = chanw,
                            ircWriteThread = threadw }

            finallyError 
               (localLB (Just chans) $ catchSignals $ m >> ircQuit "terminated") 
               (liftIO $ do 
                   killThread threadr
                   killThread threadw
                   hClose s)

        reconn <- gets ircStayConnected
        case reconn of
          True  -> runIrc' m
          False -> exitModules

  where
#if __GLASGOW_HASKELL__ >= 600
        isEOFon s (IRCRaised (IOException e)) 
            = if isEOFError e && ioeGetHandle e == Just s
              then Just () else Nothing
#else
        isEOFon s (IRCRaised e) 
            = if isEOFError e && ioeGetHandle e == Just s
              then Just () else Nothing
#endif        
        isEOFon _ _ = Nothing
        isSignal (SignalCaught s) = Just s
        isSignal _ = Nothing
        -- catches a signal, quit with message
        catchSignals n = catchErrorJust isSignal n $
             \s -> do tryError $ ircQuit (ircSignalMessage s)
                      -- forkIO $ threadDelay 1000000 >> throw ...
                      --throwError (SignalCaught s)
                      return ()

        exitModules = do
          mods <- gets $ M.elems . ircModules
          (`mapM_` mods) $ \(ModuleRef mod ref name) -> do
            -- Call ircUnloadModule?
            let ?ref = ref; ?name = name 
            moduleExit mod
            writeGlobalState mod name


readerLoop :: ThreadId -> Chan IRCMessage -> Chan IRCMessage -> Handle -> IO ()
readerLoop threadmain chanr chanw h
  = do  liftIO (putStrLn "Running reader loop...")
        exc <- try readerLoop'
        case exc of
           Left (AsyncException ThreadKilled) -> return ()
           Left err -> throwTo threadmain err
           Right _ -> return ()
  where
    readerLoop'
      = do line <- hGetLine h
           let  line' = [ c | c <- line, c /= '\n', c /= '\r' ]
           case line' of
                ('P':'I':'N':'G':' ':rest)
                    -> writeChan chanw (mkIrcMessage "PONG" [rest])
                _   -> writeChan chanr (decodeMessage line')
           readerLoop'
{-# INLINE readerLoop #-}

writerLoop :: ThreadId -> Chan IRCMessage -> Handle -> IO ()
writerLoop threadmain chanw h
  = do exc <- try writerLoop'
       case exc of
           Left (AsyncException ThreadKilled) 
             -> try (hPutStr h "QUIT :died unexpectedly\r") >> return ()
           Left e  -> throwTo threadmain e
           Right _ -> return ()
  where
    writerLoop'
      = do msg <- readChan chanw
           hPutStr h (encodeMessage msg "\r")
           writerLoop'
{-# INLINE writerLoop #-}

encodeMessage :: IRCMessage -> String -> String
encodeMessage msg
  = encodePrefix (msgPrefix msg) . encodeCommand (msgCommand msg)
          . encodeParams (msgParams msg)
  where
    encodePrefix [] = id
    encodePrefix prefix = showChar ':' . showString prefix . showChar ' '

    encodeCommand cmd = showString cmd

    encodeParams [] = id
    encodeParams (p:ps) = showChar ' ' . showString p . encodeParams ps


decodeMessage :: String -> IRCMessage
decodeMessage line
  = let (prefix, rest1) = decodePrefix (,) line in
    let (cmd, rest2)    = decodeCmd (,) rest1 in
    let params          = decodeParams rest2 in
    IRCMessage { msgPrefix = prefix, msgCommand = cmd, msgParams = params }
  where
    decodePrefix k (':':cs)
      = decodePrefix' k cs
      where
        decodePrefix' j ""       = j "" ""
        decodePrefix' j (' ':ds) = j "" ds
        decodePrefix' j (c:ds)   = decodePrefix' (\xs ys -> j (c:xs) ys) ds

    decodePrefix k cs
      = k "" cs

    decodeCmd k []
      = k "" ""
    decodeCmd k (' ':cs)
      = k "" cs
    decodeCmd k (c:cs)
      = decodeCmd (\xs ys -> k (c:xs) ys) cs

    decodeParams :: String -> [String]
    decodeParams xs
      = decodeParams' [] [] xs
      where
        decodeParams' param params []
          | null param = reverse params
          | otherwise  = reverse (reverse param : params)
        decodeParams' param params (' ' : cs)
          | null param = decodeParams' [] params cs
          | otherwise  = decodeParams' [] (reverse param : params) cs
        decodeParams' param params rest@(c@':' : cs)
          | null param = reverse (rest : params)
          | otherwise  = decodeParams' (c:param) params cs
        decodeParams' param params (c:cs)
          = decodeParams' (c:param) params cs

{-
getFirstWord :: String -> String
getFirstWord line = takeWhile (/=' ') line
-}

{-
lowQuote :: String -> String
lowQuote [] = []
lowQuote ('\0':cs)   = '\020':'0'    : lowQuote cs
lowQuote ('\n':cs)   = '\020':'n'    : lowQuote cs
lowQuote ('\r':cs)   = '\020':'r'    : lowQuote cs
lowQuote ('\020':cs) = '\020':'\020' : lowQuote cs
lowQuote (c:cs)      = c : lowQuote cs

lowDequote :: String -> String
lowDequote [] = []
lowDequote ('\020':'0'   :cs) = '\0'   : lowDequote cs
lowDequote ('\020':'n'   :cs) = '\n'   : lowDequote cs
lowDequote ('\020':'r'   :cs) = '\r'   : lowDequote cs
lowDequote ('\020':'\020':cs) = '\020' : lowDequote cs
lowDequote ('\020'       :cs) = lowDequote cs
lowDequote (c:cs)             = c : lowDequote cs

ctcpQuote :: String -> String
ctcpQuote [] = []
ctcpQuote ('\001':cs) = '\134':'a'    : ctcpQuote cs
ctcpQuote ('\134':cs) = '\134':'\134' : ctcpQuote cs
ctcpQuote (c:cs)      = c : ctcpQuote cs

ctcpDequote :: String -> String
ctcpDequote [] = []
ctcpDequote ('\134':'a'   :cs) = '\001' : ctcpDequote cs
ctcpDequote ('\134':'\134':cs) = '\134' : ctcpDequote cs
ctcpDequote ('\134':cs)        = ctcpDequote cs
ctcpDequote (c:cs)             = c : ctcpDequote cs
-}

------------------------------------------------------------------------

-- | The Module type class.
-- Minimal complete definition: @moduleHelp@, @moduleCmds@, @process@.
class Module m s | m -> s where
    -- | If the module wants its state to be saved, this function should
    --   return a Serializer.
    --
    --   The default implementation returns Nothing.
    moduleSerialize :: m -> Maybe (Serializer s)
    -- | If the module maintains state, this method specifies the default state
    --   (for example in case the state can't be read from a state).
    --
    --   The default implementation returns an error and assumes the state is 
    --   never accessed.
    moduleDefState  :: m -> LB s
    -- | This method should return a help string for every command it defines.
    moduleHelp      :: m -> String -> ModuleT s LB String
    -- | Is the module sticky? Sticky modules (as well as static ones) can't be
    --   unloaded. By default, modules are not sticky.
    moduleSticky    :: m -> Bool
    -- | The commands the module listenes to.
    moduleCmds      :: m -> ModuleT s LB [String]
    -- | The privileged commands the module listenes to.
    modulePrivs     :: m -> ModuleT s LB [String]
    -- | Initialize the module. The default implementation does nothing.
    moduleInit      :: m -> ModuleT s LB ()
    -- | Finalize the module. The default implementation does nothing.
    moduleExit      :: m -> ModuleT s LB ()
    -- | Process a command a user sent.
    process         :: m -- ^ phantom
        -> IRCMessage    -- ^ the message
        -> String        -- ^ target
        -> String        -- ^ command
        -> String        -- ^ the arguments to the command
        -> ModuleT s IRC () 

    modulePrivs _     = return []
    moduleExit _      = return ()
    moduleInit _      = return ()
    moduleSticky _    = False
    moduleSerialize _ = Nothing
    moduleDefState  _ = return $ error "state not initalized"

-- | An existential type holding a module.
data MODULE = forall m s. (Module m s) => MODULE m

data ModuleRef = forall m s. (Module m s) => ModuleRef m (MVar s) String

toFilename :: String -> String
toFilename = ("State/"++)

writeGlobalState :: Module m s => m -> String -> ModuleT s LB ()
writeGlobalState mod name = case moduleSerialize mod of
  Nothing  -> return ()
  Just ser -> do
    state <- liftIO $ readMVar ?ref -- readMS
    case serialize ser state of
      Nothing  -> return ()
      Just out -> liftIO $ writeFile (toFilename name) out

-- Read the whole file so it'll be closed
readFile' :: String -> IO String
readFile' file = do
  h <- openFile file ReadMode
  cont <- hGetContents h
  cont `deepSeq` hClose h
  return cont

readGlobalState :: Module m s => m -> String -> IO (Maybe s)
readGlobalState mod name = case moduleSerialize mod of
  Nothing  -> return Nothing
  Just ser -> do
    state <- Just `fmap` readFile' (toFilename name) `catch` \_ -> return Nothing
    return $! maybe Nothing (Just $!) $ deSerialize ser =<< state

------------------------------------------------------------------------
--
-- | Register a module in the irc state
--
ircInstallModule :: MODULE -> String -> LB ()
ircInstallModule (MODULE mod) modname = do  
    savedState <- liftIO $ readGlobalState mod modname
    state      <- maybe (moduleDefState mod) return savedState
    ref        <- liftIO $ newMVar state

    let modref = ModuleRef mod ref modname
    let ?ref = ref; ?name = modname
    moduleInit mod
    cmds <- moduleCmds mod
    privs <- modulePrivs mod

    s <- get
    let modmap = ircModules s
    let cmdmap = ircCommands s
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
    liftIO $ (fn mod sym)

--
-- | Dynamically unload a module
--
ircUnload :: FilePath -> LB ()
ircUnload mod = do
    s <- get
    liftIO $ (S.unload (ircDynLoad s)) (S.Module mod)

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
checkPrivs :: IRCMessage -> IRC Bool
checkPrivs msg = gets (isJust . M.lookup (ircNick msg) . ircPrivilegedUsers)

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
