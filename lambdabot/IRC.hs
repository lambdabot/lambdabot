{-# OPTIONS -cpp -fglasgow-exts #-}

--      $Id: IRC.hs,v 1.21 2003/07/31 19:13:15 eleganesh Exp $    

module IRC where

import BotConfig        (getMyname, getMaxLines, getAdmins, getHost, getPort)
import DeepSeq          (($!!), DeepSeq(..))
import ErrorUtils
import ExceptionError   (ExceptionError(..), ExceptionErrorT(..))
import MonadException   (MonadException(throwM, handleM))
import Util             (breakOnGlue, split, writeFM, Accessor(..))

import Map (Map)
import qualified Map as M hiding (Map)

import Network          (withSocketsDo, connectTo, PortID(PortNumber))

import System.IO        (Handle, hGetLine, hPutStr, hClose, hSetBuffering, BufferMode(NoBuffering))
-- import System.Exit      {-instances only-}

#if __GLASGOW_HASKELL__ >= 600
import System.IO.Error hiding (try)

# ifndef mingw32_HOST_OS
import System.Posix.Signals
# endif

#else
import Posix
import System.IO.Error
#endif

import Data.Char                (toLower, isAlphaNum, isSpace)
import Data.List
import Data.Dynamic             (Typeable, toDyn, fromDynamic)
import Data.IORef               (newIORef, IORef)

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

data IRCRState
  = IRCRState {
        ircServer      :: String,
        ircReadChan    :: Chan IRCMessage,
        ircReadThread  :: ThreadId,
        ircWriteChan   :: Chan IRCMessage,
        ircWriteThread :: ThreadId
  }

type Callback = IRCMessage -> IRC ()

data IRCRWState
  = IRCRWState {
        ircPrivilegedUsers :: Map String Bool,
        ircChannels        :: Map ChanName String, -- channel_name topic
        ircModules         :: Map String MODULE,
        ircCallbacks       :: Map String [Callback],
        ircCommands        :: Map String MODULE,
        ircModuleState     :: Map String (IORef ModuleState),
        ircStayConnected   :: Bool
  }

newtype ChanName = ChanName String -- should be abstract, always lowercase
  deriving (Eq, Ord)

instance Show ChanName where
  show (ChanName x) = show x

-- only use the "smart constructor":
mkCN :: String -> ChanName
mkCN = ChanName . map toLower

getCN :: ChanName -> String
getCN (ChanName c) = c

-- does the deriving Typeable do the right thing?
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
  = Catch $ throwTo threadid $ DynException $ toDyn $ SignalException s
#endif  

withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m 
  = do threadid <- liftIO $ myThreadId
       withHandlerList ircSignalsToCatch (ircSignalHandler threadid) Nothing m

data IRCError = IRCRaised Exception 
              | SignalCaught Signal 
  deriving Show


ircErrorMsg :: IRCError -> String
ircErrorMsg (IRCRaised e) = show e
ircErrorMsg (SignalCaught s) = "caught signal "++show s

instance ExceptionError IRCError where
  fromException e 
    = case e of
        DynException d ->
          case fromDynamic d of
             Just (SignalException s) -> SignalCaught s
             Nothing -> IRCRaised e
        _ -> IRCRaised e


type IRCErrorT m a = ExceptionErrorT IRCError m a

handleIRCErrorT :: (MonadIO m,MonadException m) => IRCErrorT m () -> m ()
handleIRCErrorT m 
  = do res <- runExceptionErrorT m
       case res of
         Left (IRCRaised e) -> throwM e
#ifndef mingw32_HOST_OS         
         Left (SignalCaught s) -> liftIO $ raiseSignal s
#endif         
-- overlapped for now, but won't be if IRCError gets extended
--       Left e -> throwM $ ErrorCall (ircErrorMsg e)
         Right v -> return v


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

ircnick     :: IRCMessage -> String
ircnick msg = fst $ breakOnGlue "!" (msgPrefix msg)

ircuser     :: IRCMessage -> String
ircuser msg = head $ split "@" $ (split "!" (msgPrefix msg)) !! 1

irchost     :: IRCMessage -> String
irchost msg = (split "@" (msgPrefix msg)) !! 1

data ModuleState = forall a. (Typeable a) => ModuleState a

-- in monad LB we don't have a connection
newtype LB a = LB { runLB :: IRCErrorT (StateT IRCRWState IO) a }
#if __GLASGOW_HASKELL__ >= 600
   deriving (Monad,MonadState IRCRWState,MonadError IRCError,MonadIO)
#else
instance Monad LB where
  return a = LB $ return a
  LB m >>= f = LB $ do v <- m
                       runLB (f v)

instance MonadState IRCRWState LB where
  get = LB get
  put s = LB $ put s

instance MonadError IRCError LB where
  throwError e = LB $ throwError e
  catchError (LB m) f = LB $ catchError m (runLB . f)

instance MonadIO LB where
  liftIO m = LB $ liftIO m

#endif

evalLB :: LB () -> IRCRWState -> IO ()
evalLB lb rws = evalStateT (handleIRCErrorT (runLB lb)) rws

class (Monad m,MonadState IRCRWState m,MonadError IRCError m,MonadIO m) 
   => MonadLB m 
   where
  liftLB :: LB a -> m a

newtype IRC a 
  = IRC { runIRC :: IRCErrorT (ReaderT IRCRState (StateT IRCRWState IO)) a }
#if __GLASGOW_HASKELL__ >= 600
    deriving (Monad,MonadReader IRCRState,MonadState IRCRWState,
              MonadError IRCError,MonadIO)
#else
instance Monad IRC where
  return a = IRC $ return a
  IRC m >>= f = IRC $ do v <- m
                         runIRC (f v)

instance MonadReader IRCRState IRC where
  ask = IRC ask
  local f (IRC m) = IRC $ local f m

instance MonadState IRCRWState IRC where
  get = IRC get
  put s = IRC $ put s
  
instance MonadError IRCError IRC where
  throwError e = IRC $ throwError e
  catchError (IRC m) f = IRC $ catchError m (runIRC . f)

instance MonadIO IRC where
  liftIO m = IRC $ liftIO m

#endif

instance MonadLB LB where
  liftLB m = m

instance MonadLB IRC where
  liftLB (LB (ExceptionErrorT m)) 
   = IRC $ ExceptionErrorT $ lift m

mkIrcMessage :: String -> [String] -> IRCMessage
mkIrcMessage cmd params
  = IRCMessage { msgPrefix = "", msgCommand = cmd, msgParams = params }

ircSignOn :: String -> String -> IRC ()
ircSignOn nick ircname
  = do  server <- asks ircServer
        ircWrite (mkIrcMessage "USER" [nick, "localhost", server, ircname])
        ircWrite (mkIrcMessage "NICK" [nick])

ircGetChannels :: IRC [String]
ircGetChannels
  = do  chans <- gets ircChannels
        return $ map getCN (M.keys chans)

-- evil hack to make the MoreModule work
-- change this to an output filter when the new Module typeclass arrives
ircPrivmsg :: String -> String -> IRC ()
ircPrivmsg who msg
    = do myname <- getMyname
         maxLines <- getMaxLines
         if (who /= myname) then
          do let msglines  = mlines msg
                 morelines = drop maxLines msglines
                 thislines = take maxLines msglines
                 sendlines = if (length morelines > 0)
                             then thislines ++ ["[" ++ show (length morelines) 
                                            ++ " @more lines]"]
                             else thislines
             moreStateSet $ unlines morelines
             mapM_ (ircPrivmsg' who) sendlines
          else return ()

{-# INLINE ircPrivmsg #-}

------------------------------------------------------------------------

{- yes it's ugly, but... -}

mlines			:: String -> [String]
mlines ""		=  []
mlines s		=  let (l, s') = mbreak (0::Int) (== '\n') s
			   in  l : case s' of
					[]  -> []
					s'' -> mlines s''
{-# INLINE mlines #-}

mbreak :: (Num a, Ord a) => a -> (Char -> Bool) -> [Char] -> ([Char], [Char])
mbreak _ _ xs@[] = (xs, xs)
mbreak n p xs@(x:xs')
    | n == 80  =  ([],dropWhile isSpace xs)
    | n > 70 && not (isAlphaNum x) = ([x], dropWhile isSpace xs')
    | p x	=  ([],xs')
    | otherwise	=  let (ys,zs) = mbreak (n+1) p xs' in (x:ys,zs)
{-# INLINE mbreak #-}

{- yes it's ugly, but... -}


ircPrivmsg' :: String -> String -> IRC ()
ircPrivmsg' who msg
  = ircWrite (mkIrcMessage "NOTICE" [who, ':' : clean_msg])
    -- merry christmas det
    where clean_msg = concatMap clean msg

--clean x | x `elem` specials = ['\\',x]
clean :: Char -> [Char]
clean x | x == '\CR' = []
        | otherwise         = [x]
        -- where specials = "\\"

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
ircQuit msg = do state <- get
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

ircRead :: IRC IRCMessage
ircRead
  = do  chanr <- asks ircReadChan
        liftIO (readChan chanr)

ircWrite :: IRCMessage -> IRC ()
ircWrite line
  = do  chanw <- asks ircWriteChan
        -- use DeepSeq's $!! to ensure that any Haskell errors in line
        -- are caught now, rather than later on in the other thread
        liftIO (writeChan chanw $!! line)

-- may wish to add more things to the things caught, or restructure things 
-- a bit. Can't just catch everything - in particular EOFs from the socket
-- loops get thrown to this thread and we musn't just ignore them.
handleIrc :: (String -> IRC ()) -> IRC () -> IRC ()
handleIrc handler m 
  = catchError m
               (\e -> case e of
                        IRCRaised (ErrorCall s) -> handler s
                        IRCRaised (PatternMatchFail s) -> handler s
                        _ -> throwError e)


runIrc :: LB () -> IRC () -> IO ()
runIrc ini m
  = withSocketsDo $
       do admins <- getAdmins
          exc <- try $ evalLB
                         (ini >> withIrcSignalCatch (runIrc' m))
                         (initState admins)
          case exc of
            Left exception -> putStrLn ("Exception: " ++ show exception)
            Right result   -> return result
      where
        initState admins' 
                  = IRCRWState {
                        ircPrivilegedUsers 
                            = M.fromList [ (user,True) | user <- admins' ],
                        ircChannels = M.empty,
                        ircModules = M.empty,
                        ircCallbacks = M.empty,
                        ircCommands = M.empty,
                        ircModuleState = M.empty,
                        ircStayConnected = True
                    }

traceError :: (MonadIO m,MonadError e m,Show e) => m a -> m a
traceError = handleError (\e -> liftIO (print e) >> throwError e)

traceException :: (MonadIO m,MonadException m) => m a -> m a
traceException = handleM (\e -> liftIO (print e) >> throwM e)



runIrc' :: IRC () -> LB ()
runIrc' m 
  = do  hostname <- getHost
        portnum <- liftM (fromIntegral . (read :: String -> Integer)) getPort
        s <- liftIO $ connectTo hostname (PortNumber portnum)
        tryErrorJust (isEOFon s) $
         do liftIO $ hSetBuffering s NoBuffering
            threadmain <- liftIO $ myThreadId
            chanr <- liftIO $ newChan
            chanw <- liftIO $ newChan
            threadr <- liftIO $ forkIO $ readerLoop threadmain chanr chanw s
            threadw <- liftIO $ forkIO $ writerLoop threadmain chanw s
            let { chans = IRCRState {
                    ircServer      = hostname,
                    ircReadChan    = chanr,
                    ircReadThread  = threadr,
                    ircWriteChan   = chanw,
                    ircWriteThread = threadw
              } }
            finallyError 
              (LB $ ExceptionErrorT $
                 runReaderT (runExceptionErrorT $ runIRC $ catchSignals $
                                               m >> ircQuit "terminated")
                            chans
              )
              (do 
                  liftIO $ killThread threadr
                  liftIO $ killThread threadw
                  liftIO $ hClose s)

        reconn <- gets ircStayConnected
        if reconn then runIrc' m else return ()

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
        catchSignals n = catchErrorJust isSignal n
                             (\s -> do tryError $ ircQuit (ircSignalMessage s)
                                       throwError (SignalCaught s))
                                    

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


getFirstWord :: String -> String
getFirstWord line = takeWhile (/=' ') line


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

class Module m where
    moduleName      :: m -> LB String
    moduleHelp      :: m -> String -> LB String
    moduleSticky    :: m -> Bool
    commands        :: m -> LB [String]
    moduleInit      :: m -> LB ()
    moduleExit      :: m -> LB ()
    process         :: m -> IRCMessage -> String -> String -> String -> IRC () -- msg target cmd rest
    moduleInit _  = return ()
    moduleExit _  = return ()

data MODULE = forall m. (Module m) => MODULE m

--
-- | register a module in the irc state
--
ircInstallModule :: Module m => m -> LB ()
ircInstallModule modn
  = do  s <- get
        modname <- moduleName modn
        let modmap = ircModules s
        put (s { ircModules = M.insert modname (MODULE modn)  modmap })
        ircLoadModule modname

ircLoadModule :: String -> LB ()
ircLoadModule modname
  = do  maybemod   <- gets (\s -> M.lookup modname (ircModules s))
        case maybemod of
            Just (MODULE m) -> do ircLoadModule' m
                                  moduleInit m
            Nothing         -> return ()
  where
    ircLoadModule' m
      = do  cmds <- commands m
            s <- get
            let cmdmap = ircCommands s        -- :: Map String MODULE
            put (s { ircCommands = M.addList [ (cmd,(MODULE m)) | cmd <- cmds ] cmdmap })

ircUnloadModule :: String -> LB ()
ircUnloadModule modname
    = do maybemod <- gets (\s -> M.lookup modname (ircModules s))
         case maybemod of
                       Just (MODULE m)
                         -> if moduleSticky m 
                            then error "module is sticky"
                            else do moduleExit m
                                    ircUnloadModule' m
                       _ -> error "module not loaded"
    where
    ircUnloadModule' m
        = do modnm <- moduleName m
             cmds  <- commands m
             s <- get
             let modmap = ircModules s        -- :: Map String MODULE,
                 cmdmap = ircCommands s        -- :: Map String MODULE
                 in
                 put (s { ircCommands = foldl (flip M.delete) cmdmap cmds }
                        { ircModules = M.delete modnm modmap })


ircSignalConnect :: String -> (IRCMessage -> IRC ()) -> LB ()
ircSignalConnect str f 
    = do s <- get
         let cbs = (ircCallbacks s)
         case (M.lookup str cbs) of 
              Nothing -> put (s { ircCallbacks = M.insert str [f]    cbs }) 
              Just fs -> put (s { ircCallbacks = M.insert str (f:fs) cbs}) 



--isAdmin     :: IRCMessage -> Bool
checkPrivs :: IRCMessage -> String -> IRC () -> IRC ()
checkPrivs msg target f = do
                          maybepriv <- gets (\s -> M.lookup (ircnick msg) (ircPrivilegedUsers s) )
                          case maybepriv of
                                         Just _  -> f
                                         Nothing -> ircPrivmsg target "not enough privileges"

stripMS :: (Typeable a) => ModuleState -> a
stripMS (ModuleState x) = case fromDynamic $ toDyn $ x of
                            Nothing -> error $ "wrong type ("++(show $ toDyn x)++") in ModuleState"
                            Just y -> y

-- this belongs in the MoreModule, but causes cyclic imports

moreStateSet :: (MonadIO m, MonadState IRCRWState m, Typeable a) => a -> m ()
moreStateSet lns =
    do s <- get
       newRef <- liftIO . newIORef $ ModuleState lns
       let stateMap = ircModuleState s
       put (s { ircModuleState = M.insert "more" newRef stateMap })

ircModuleStateAccessor :: MonadState IRCRWState m 
                       => Accessor m (Map String (IORef ModuleState))
ircModuleStateAccessor 
 = Accessor { reader = gets ircModuleState,
              writer = \v -> do s <- get
                                put (s {ircModuleState = v}) }
                    
makeInitialState :: Typeable a => String -> a -> LB ()
makeInitialState modname initState 
    = do newRef <- liftIO . newIORef $ ModuleState initState
         writeFM ircModuleStateAccessor modname newRef

