module IRC where
--      $Id: IRC.hs,v 1.21 2003/07/31 19:13:15 eleganesh Exp $    
import Network
import Monad
import Maybe
import GHC.IO
import GHC.IOBase (Handle, BufferMode(..))
import GHC.Handle
import System.Exit
#if __GLASGOW_HASKELL__ >= 600
import System.IO.Error hiding (try)
#else
import System.IO.Error
#endif
#if __GLASGOW_HASKELL__ >= 600
import System.Posix.Signals
#else
import Posix
#endif
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error (MonadError (..))
import Data.Char
import Data.List
import Data.FiniteMap
import Data.Dynamic
import Data.IORef
import BotConfig
import DeepSeq
import Util
import MonadException
import ExceptionError
import ErrorUtils

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
        ircPrivilegedUsers :: FiniteMap String Bool,
        ircChannels        :: FiniteMap String String, -- channel_name topic
        ircModules         :: FiniteMap String MODULE,
        ircCallbacks       :: FiniteMap String [Callback],
        ircCommands        :: FiniteMap String MODULE,
        ircModuleState     :: FiniteMap String (IORef ModuleState),
        ircStayConnected   :: Bool
  }

-- does the deriving Typeable do the right thing?
newtype SignalException = SignalException Signal
  deriving Typeable

withHandler :: (MonadIO m,MonadError e m)
            => Signal 
            -> Handler 
            -> Maybe SignalSet 
            -> m () 
            -> m ()
withHandler s h ss m 
  = bracketError (liftIO $ installHandler s h ss)
                 (\oldh -> liftIO $ installHandler s oldh Nothing)
                 (\_ -> m)

withHandlerList :: (MonadError e m,MonadIO m)
                => [Signal]
                -> (Signal -> Handler) 
                -> Maybe SignalSet 
                -> m () 
                -> m ()
withHandlerList sl h ss m = foldr (\s -> withHandler s (h s) ss) m sl

-- be careful adding signals, some signals can't be caught and installHandler
-- just raises an exception if you try
ircSignalsToCatch = [
#if __GLASGOW_HASKELL__ >= 600
                     busError,
#endif
                     segmentationViolation,
                     keyboardSignal,softwareTermination,
                     keyboardTermination,lostConnection]

ircSignalMessage s 
#if __GLASGOW_HASKELL__ >= 600
                   | s==busError = "killed by SIGBUS"
#endif
                   | s==segmentationViolation = "killed by SIGSEGV"
                   | s==keyboardSignal = "killed by SIGINT"
                   | s==softwareTermination = "killed by SIGTERM"
                   | s==keyboardTermination = "killed by SIGQUIT"
                   | s==lostConnection = "killed by SIGHUP"
 -- this case shouldn't happen if the list of messages is kept up to date
 -- with the list of signals caught
                   | otherwise = "killed by unknown signal"

ircSignalHandler threadid s
  = Catch $ throwTo threadid $ DynException $ toDyn $ SignalException s

withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m 
  = do threadid <- liftIO $ myThreadId
       withHandlerList ircSignalsToCatch (ircSignalHandler threadid) Nothing m

data IRCError = IRCRaised Exception 
              | SignalCaught Signal 
  deriving Show

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
         Left (SignalCaught s) -> liftIO $ raiseSignal s
-- overlapped for now, but won't be if IRCError gets extended
         Left e -> throwM $ ErrorCall (ircErrorMsg e)
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
        return (keysFM chans)

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
                 sendlines = if ((length morelines) > 1)
                             then thislines ++ ["[" ++ show (length morelines) 
                                            ++ " @more lines]"]
                             else thislines
             moreStateSet $ unlines morelines
             mapM_ (ircPrivmsg' who) sendlines
          else return ()

{- yes it's ugly, but... -}

mlines			:: String -> [String]
mlines ""		=  []
mlines s		=  let (l, s') = mbreak 0 (== '\n') s
			   in  l : case s' of
					[]     	-> []
					(_:s'') -> mlines s''

mbreak _ _ xs@[]		=  (xs, xs)
mbreak n p xs@(x:xs')
    | n == 80  =  ([],xs)
    | n > 70 && not (isAlphaNum x) = ([], xs)
    | p x	=  ([],xs)
    | otherwise	=  let (ys,zs) = mbreak (n+1) p xs' in (x:ys,zs)

{- yes it's ugly, but... -}


ircPrivmsg' :: String -> String -> IRC ()
ircPrivmsg' who msg
  = ircWrite (mkIrcMessage "PRIVMSG" [who, ':' : clean_msg])
    -- merry christmas det
    where clean_msg = concatMap clean msg

--clean x | x `elem` specials = ['\\',x]
clean x | x == '\CR'        = []
        | otherwise         = [x]
        where
        specials = "\\"

ircTopic :: String -> String -> IRC ()
ircTopic chan topic
  = ircWrite (mkIrcMessage "TOPIC" [chan, ':' : topic])

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
runIrc init m
  = withSocketsDo $
       do admins <- getAdmins
          exc <- try $ evalLB
                         (init >> withIrcSignalCatch (runIrc' m))
                         (initState admins)
          case exc of
            Left exception -> putStrLn ("Exception: " ++ show exception)
            Right result   -> return result
      where
        initState admins' 
                  = IRCRWState {
                        ircPrivilegedUsers 
                            = listToFM [ (user,True) | user <- admins' ],
                        ircChannels = emptyFM,
                        ircModules = emptyFM,
                        ircCallbacks = emptyFM,
                        ircCommands = emptyFM,
                        ircModuleState = emptyFM,
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
        isEOFon s _ = Nothing
        isSignal (SignalCaught s) = Just s
        isSignal _ = Nothing
        catchSignals m = catchErrorJust isSignal m 
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
        decodePrefix' k ""
          = k "" ""
        decodePrefix' k (' ':cs)
          = k "" cs
        decodePrefix' k (c:cs)
          = decodePrefix' (\xs ys -> k (c:xs) ys) cs
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
    moduleSticky    :: m -> Bool
    commands        :: m -> LB [String]
    moduleInit      :: m -> LB ()
    moduleExit      :: m -> LB ()
    process         :: m -> IRCMessage -> String -> String -> String -> IRC () -- msg target cmd rest
    moduleInit _  = return ()
    moduleExit _  = return ()

data MODULE = forall m. (Module m) => MODULE m

ircInstallModule :: Module m => m -> LB ()
ircInstallModule mod
  = do  s <- get
        modname <- moduleName mod
        let modmap = ircModules s
        put (s { ircModules = addToFM modmap modname (MODULE mod) })
        ircLoadModule modname

ircLoadModule :: String -> LB ()
ircLoadModule modname
  = do  maybemod   <- gets (\s -> lookupFM (ircModules s) modname)
        case maybemod of
            Just (MODULE m) -> do ircLoadModule' m
                                  moduleInit m
            Nothing         -> return ()
  where
    ircLoadModule' m
      = do  cmds <- commands m
            s <- get
            let cmdmap = ircCommands s        -- :: FiniteMap String MODULE
            put (s { ircCommands = addListToFM cmdmap [ (cmd,(MODULE m)) | cmd <- cmds ] })

ircUnloadModule :: String -> LB ()
ircUnloadModule modname
    = do maybemod <- gets (\s -> lookupFM (ircModules s) modname)
         case maybemod of
                       Just (MODULE m)
                         -> if moduleSticky m 
                            then error "module is sticky"
                            else do moduleExit m
                                    ircUnloadModule' m
                       _ -> error "module not loaded"
    where
    ircUnloadModule' m
        = do modname <- moduleName m
             cmds    <- commands m
             s <- get
             let modmap = ircModules s        -- :: FiniteMap String MODULE,
                 cmdmap = ircCommands s        -- :: FiniteMap String MODULE
                 in
                 put (s { ircCommands = delListFromFM cmdmap cmds }
                        { ircModules = delFromFM modmap modname })


ircSignalConnect :: String -> (IRCMessage -> IRC ()) -> LB ()
ircSignalConnect str f 
    = do s <- get
         let cbs = (ircCallbacks s)
         case (lookupFM cbs str) of 
              Nothing -> put (s { ircCallbacks = addToFM cbs str [f]}) 
              Just fs -> put (s { ircCallbacks = addToFM cbs str (f:fs)}) 



--isAdmin     :: IRCMessage -> Bool
checkPrivs msg target f = do 
                          maybepriv <- gets (\s -> lookupFM (ircPrivilegedUsers s) (ircnick msg))
                          case maybepriv of
                                         Just x  -> f
                                         Nothing -> ircPrivmsg target "not enough privileges"

stripMS (ModuleState x) = case fromDynamic $ toDyn $ x of
                            Nothing -> error $ "wrong type ("++(show $ toDyn x)++") in ModuleState"
                            Just y -> y

-- this belongs in the MoreModule, but causes cyclic imports

moreStateSet lines = 
    do s <- get
       newRef <- liftIO . newIORef $ ModuleState lines
       let stateMap = ircModuleState s
       put (s { ircModuleState = 
                addToFM stateMap "more" newRef })

ircModuleStateAccessor :: MonadState IRCRWState m 
                       => Accessor m (FiniteMap String (IORef ModuleState))
ircModuleStateAccessor 
 = Accessor { reader = gets ircModuleState,
              writer = \v -> do s <- get
                                put (s {ircModuleState = v}) }
                    
makeInitialState :: Typeable a => String -> a -> LB ()
makeInitialState modname initState 
    = do newRef <- liftIO . newIORef $ ModuleState initState
         writeFM ircModuleStateAccessor modname newRef

