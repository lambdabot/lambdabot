{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Lambdabot.Monad
    ( IRCRState(..)
    , initRoState
    
    , Callback
    , ModuleRef(..)
    , CommandRef(..)
    , IRCRWState(..)
    , initRwState
    
    , IRCError(..)
    
    , LB(..)
    , MonadLB(..)
    , lbIO
    , evalLB
    
    , send
    , addServer
    , remServer
    
    , handleIrc
    , catchIrc
    
    , forkLB
    , liftLB
    
    , getConfig
    
    , withModule
    , withCommand
    , withAllModules
    
    , verbose
    , proxy
    , ghci
    ) where

import           Lambdabot.ChanName
import           Lambdabot.Command
import           Lambdabot.Config
import           Lambdabot.Config.Core
import           Lambdabot.IRC (IrcMessage)
import           Lambdabot.Module
import qualified Lambdabot.Message as Msg
import           Lambdabot.Nick
import           Lambdabot.OutputFilter
import           Lambdabot.Util.Signals
import           Lambdabot.Util

import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Control.Exception as E (catch)
import Control.Monad.Error (MonadError (..))
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Dependent.Map as D
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.Haskeline.MonadException (MonadException)
import System.IO

------------------------------------------------------------------------
--
-- Lambdabot state
--

-- | Global read-only state.
data IRCRState = IRCRState
    { ircInitDoneMVar   :: MVar ()
    , ircQuitMVar       :: MVar ()
    , ircConfig         :: D.DMap Config
    }

-- | Default ro state
initRoState :: D.DMap Config -> IO IRCRState
initRoState configuration = do
    quitMVar     <- newEmptyMVar
    initDoneMVar <- newEmptyMVar
    
    return IRCRState 
        { ircQuitMVar       = quitMVar
        , ircInitDoneMVar   = initDoneMVar
        , ircConfig         = configuration
        }

type Callback = IrcMessage -> LB ()

data ModuleRef = forall st.
    ModuleRef (Module st) (MVar st) String

data CommandRef = forall st.
    CommandRef (Module st) (MVar st) (Command (ModuleT st LB)) String

-- | Global read\/write state.
data IRCRWState = IRCRWState
    { ircServerMap       :: M.Map String (String, IrcMessage -> LB ())
    , ircPrivilegedUsers :: S.Set Nick
    , ircIgnoredUsers    :: S.Set Nick
    
    , ircChannels        :: M.Map ChanName String
    -- ^ maps channel names to topics
    
    , ircModules         :: M.Map String ModuleRef
    , ircCallbacks       :: M.Map String [(String,Callback)]
    , ircOutputFilters   :: [(String, OutputFilter LB)]
    -- ^ Output filters, invoked from right to left
    
    , ircCommands        :: M.Map String CommandRef
    , ircStayConnected   :: !Bool
    }

-- | Default rw state
initRwState :: IRCRWState
initRwState = IRCRWState
    { ircPrivilegedUsers = S.singleton (Nick "offlinerc" "null")
    , ircIgnoredUsers    = S.empty
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


-- The virtual chat system.
--
-- The virtual chat system sits between the chat drivers and the rest of
-- Lambdabot.  It provides a mapping between the String server "tags" and
-- functions which are able to handle sending messages.
--
-- When a message is recieved, the chat module is expected to call
-- `Lambdabot.Main.received'.  This is not ideal.

addServer :: String -> (IrcMessage -> LB ()) -> ModuleT mod LB ()
addServer tag sendf = do
    s <- get
    let svrs = ircServerMap s
    name <- getModuleName
    case M.lookup tag svrs of
        Nothing -> put (s { ircServerMap = M.insert tag (name,sendf) svrs})
        Just _ -> fail $ "attempted to create two servers named " ++ tag

remServer :: String -> LB ()
remServer tag = do
    s <- get
    let svrs = ircServerMap s
    case M.lookup tag svrs of
        Just _ -> do
            let svrs' = M.delete tag svrs
            put (s { ircServerMap = svrs' })
            when (M.null svrs') $ do
                quitMVar <- asks ircQuitMVar
                io $ putMVar quitMVar ()
        Nothing -> fail $ "attempted to delete nonexistent servers named " ++ tag

send :: IrcMessage -> LB ()
send msg = do
    s <- gets ircServerMap
    case M.lookup (Msg.server msg) s of
        Just (_, sendf) -> sendf msg
        Nothing -> io $ hPutStrLn stderr $ "sending message to bogus server: " ++ show msg


-- ---------------------------------------------------------------------
--
-- The LB (LambdaBot) monad
--

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
--
-- instances Monad, Functor, MonadIO, MonadState, MonadError


newtype LB a = LB { runLB :: ReaderT (IRCRState,IORef IRCRWState) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)

class (MonadIO m, MonadConfig m, Applicative m) => MonadLB m where
    lb :: LB a -> m a

instance MonadLB LB where lb = id
instance MonadLB m => MonadLB (ModuleT st m) where lb = lift . lb
instance MonadLB m => MonadLB (Cmd m)        where lb = lift . lb

-- Actually, this isn't a reader anymore
instance MonadReader IRCRState LB where
    ask   = LB $ asks fst
    local = error "You are not supposed to call local"

instance MonadState IRCRWState LB where
    get = LB $ do
        ref <- asks snd
        lift $ readIORef ref
    put x = LB $ do
        ref <- asks snd
        lift $ writeIORef ref x

-- And now a MonadError instance to map IRCErrors to MonadError in LB,
-- so throwError and catchError "just work"
instance MonadError IRCError LB where
  throwError (IRCRaised e)    = io $ throwIO e
  throwError (SignalCaught e) = io $ evaluate (throw $ SignalException e)
  m `catchError` h = lbIO $ \conv -> (conv m
              `E.catch` \(SignalException e) -> conv $ h $ SignalCaught e)
              `E.catch` \e -> conv $ h $ IRCRaised e

instance MonadConfig LB where
    getConfig k = liftM (maybe (getConfigDefault k) id . D.lookup k) (lb (asks ircConfig))

-- A type for handling both Haskell exceptions and external signals
data IRCError = IRCRaised SomeException | SignalCaught Signal

instance Show IRCError where
    show (IRCRaised    e) = show e
    show (SignalCaught s) = show s

-- lbIO return :: LB (LB a -> IO a)
-- CPS to work around predicativiy of haskell's type system.
lbIO :: MonadLB m => ((forall a. LB a -> IO a) -> IO b) -> m b
lbIO k = lb $ do
    r <- LB ask
    io (k (flip runReaderT r . runLB))

-- | run a computation in the LB monad
evalLB :: LB a -> IRCRState -> IRCRWState -> IO a
evalLB (LB lb') rs rws = do
    ref  <- newIORef rws
    lb' `runReaderT` (rs,ref)

-- May wish to add more things to the things caught, or restructure things
-- a bit. Can't just catch everything - in particular EOFs from the socket
-- loops get thrown to this thread and we musn't just ignore them.
handleIrc :: MonadError IRCError m => (IRCError -> m ()) -> m () -> m ()
handleIrc handler m = catchError m handler

-- Like handleIrc, but with arguments reversed
catchIrc :: MonadError IRCError m => m () -> (IRCError -> m ()) -> m ()
catchIrc = flip handleIrc

-- | run an IO action in another thread, with a timeout, lifted into LB
forkLB :: LB a -> LB ThreadId
forkLB f = (`liftLB` f) $ \g -> do
             forkIO $ do
               _ <- timeout (15 * 1000 * 1000) g
               return ()

-- | lift an io transformer into LB
liftLB :: (IO a -> IO b) -> LB a -> LB b
liftLB f = LB . mapReaderT f . runLB -- lbIO (\conv -> f (conv lb))

------------------------------------------------------------------------
-- Module handling

-- | Interpret an expression in the context of a module.
-- Arguments are which map to use (@ircModules@ and @ircCommands@ are
-- the only sensible arguments here), the name of the module\/command,
-- action for the case that the lookup fails, action if the lookup
-- succeeds.
--
withModule :: String
           -> LB a
           -> (forall st. Module st -> ModuleT st LB a)
           -> LB a

withModule modname def f = do
    maybemod <- gets (M.lookup modname . ircModules)
    case maybemod of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (ModuleRef m ref name) -> do
          runReaderT (moduleT $ f m) (ref, name)
      _                           -> def

withCommand :: String
            -> LB a
            -> (forall st. Module st
                        -> Command (ModuleT st LB)
                        -> ModuleT st LB a)
            -> LB a

withCommand cmdname def f = do
    maybecmd <- gets (M.lookup cmdname . ircCommands)
    case maybecmd of
      -- TODO stick this ref stuff in a monad instead. more portable in
      -- the long run.
      Just (CommandRef m ref cmd name) -> do
          runReaderT (moduleT $ f m cmd) (ref, name)
      _                           -> def

-- | Interpret a function in the context of all modules
withAllModules :: (forall st. Module st -> ModuleT st LB a) -> LB [a]
withAllModules f = do
    mods <- gets $ M.elems . ircModules :: LB [ModuleRef]
    (`mapM` mods) $ \(ModuleRef m ref name) -> do
        runReaderT (moduleT $ f m) (ref, name)
