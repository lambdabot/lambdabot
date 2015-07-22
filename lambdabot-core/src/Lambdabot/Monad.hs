{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lambdabot.Monad
    ( IRCRState
    , initRoState
    , reportInitDone
    , waitForInit
    , waitForQuit
    
    , Callback
    , ModuleRef(..)
    , CommandRef(..)
    , IRCRWState(..)
    , initRwState
    
    , LB(..)
    , MonadLB(..)
    , evalLB
    
    , addServer
    , remServer
    , send
    , received
    
    , getConfig
    
    , withModule
    , withCommand
    , withAllModules
    ) where

import           Lambdabot.ChanName
import           Lambdabot.Command
import           Lambdabot.Config
import           Lambdabot.Config.Core
import           Lambdabot.IRC
import           Lambdabot.Logging
import           Lambdabot.Module
import qualified Lambdabot.Message as Msg
import           Lambdabot.Nick
import           Lambdabot.OutputFilter
import           Lambdabot.Util

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Exception.Lifted as E (catch)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import qualified Data.Dependent.Map as D
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.Haskeline.MonadException (MonadException)

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
initRoState :: [D.DSum Config] -> IO IRCRState
initRoState configuration = do
    quitMVar     <- newEmptyMVar
    initDoneMVar <- newEmptyMVar
    
    return IRCRState 
        { ircQuitMVar       = quitMVar
        , ircInitDoneMVar   = initDoneMVar
        , ircConfig         = D.fromListWithKey (flip . mergeConfig) configuration
        }

reportInitDone :: MonadIO m => IRCRState -> m ()
reportInitDone = io . flip putMVar () . ircInitDoneMVar

askLB :: MonadLB m => (IRCRState -> a) -> m a
askLB f  = lb . LB $ asks (f . fst)

waitForInit :: MonadLB m => m ()
waitForInit = readMVar =<< askLB ircInitDoneMVar

waitForQuit :: MonadLB m => m ()
waitForQuit = readMVar =<< askLB ircQuitMVar

type Callback = IrcMessage -> LB ()

data ModuleRef = forall st.
    ModuleRef (Module st) (MVar st) String

data CommandRef = forall st.
    CommandRef (Module st) (MVar st) String (Command (ModuleT st LB))

-- | Global read\/write state.
data IRCRWState = IRCRWState
    { ircServerMap       :: M.Map String (String, IrcMessage -> LB ())
    , ircPrivilegedUsers :: S.Set Nick
    , ircIgnoredUsers    :: S.Set Nick
    
    , ircChannels        :: M.Map ChanName String
    -- ^ maps channel names to topics
    , ircPersists        :: M.Map String Bool
    -- ^ lists servers to which try to reconnect on failure (one-time or always)
    
    , ircModules         :: M.Map String ModuleRef
    , ircCallbacks       :: M.Map String [(String,Callback)]
    , ircOutputFilters   :: [(String, OutputFilter LB)]
    -- ^ Output filters, invoked from right to left
    
    , ircCommands        :: M.Map String CommandRef
    }

-- | Default rw state
initRwState :: IRCRWState
initRwState = IRCRWState
    { ircPrivilegedUsers = S.singleton (Nick "offlinerc" "null")
    , ircIgnoredUsers    = S.empty
    , ircChannels        = M.empty
    , ircPersists        = M.empty
    , ircModules         = M.empty
    , ircServerMap       = M.empty
    , ircCallbacks       = M.empty
    , ircOutputFilters   = 
        [ ([],cleanOutput)
        , ([],lineify)
        , ([],cleanOutput)
        ]
    , ircCommands        = M.empty
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
    s <- lift get
    let svrs = ircServerMap s
    name <- getModuleName
    case M.lookup tag svrs of
        Nothing -> lift (put s { ircServerMap = M.insert tag (name,sendf) svrs})
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
                quitMVar <- askLB ircQuitMVar
                io $ putMVar quitMVar ()
        Nothing -> fail $ "attempted to delete nonexistent servers named " ++ tag

send :: IrcMessage -> LB ()
send msg = do
    s <- gets ircServerMap
    case M.lookup (Msg.server msg) s of
        Just (_, sendf) -> sendf msg
        Nothing -> warningM $ "sending message to bogus server: " ++ show msg

received :: IrcMessage -> LB ()
received msg = do
    s       <- get
    handler <- getConfig uncaughtExceptionHandler
    case M.lookup (ircMsgCommand msg) (ircCallbacks s) of
        Just cbs -> mapM_ (\(_, cb) -> cb msg `E.catch` (liftIO . handler)) cbs
        _        -> return ()

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

instance MonadBase IO LB where
    liftBase = LB . liftBase

instance MonadBaseControl IO LB where
    type StM LB a = StM (ReaderT (IRCRState,IORef IRCRWState) IO) a
    liftBaseWith action = LB (liftBaseWith (\run -> action (run . runLB)))
    restoreM = LB . restoreM

class (MonadIO m, MonadBaseControl IO m, MonadConfig m, MonadLogging m, Applicative m) => MonadLB m where
    lb :: LB a -> m a

instance MonadLB LB where lb = id
instance MonadLB m => MonadLB (ModuleT st m) where lb = lift . lb
instance MonadLB m => MonadLB (Cmd m)        where lb = lift . lb

instance MonadState IRCRWState LB where
    get = LB $ do
        ref <- asks snd
        lift $ readIORef ref
    put x = LB $ do
        ref <- asks snd
        lift $ writeIORef ref x

instance MonadConfig LB where
    getConfig k = liftM (maybe (getConfigDefault k) id . D.lookup k) (lb (askLB ircConfig))

instance MonadLogging LB where
    getCurrentLogger = getConfig lbRootLoggerPath
    logM a b c = io (logM a b c)

-- | run a computation in the LB monad
evalLB :: LB a -> IRCRState -> IRCRWState -> IO a
evalLB (LB lb') rs rws = do
    ref  <- newIORef rws
    lb' `runReaderT` (rs,ref)

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
          runReaderT (runModuleT $ f m) (ref, name)
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
      Just (CommandRef m ref name cmd) -> do
          runReaderT (runModuleT $ f m cmd) (ref, name)
      _                           -> def

-- | Interpret a function in the context of all modules
withAllModules :: (forall st. Module st -> ModuleT st LB a) -> LB ()
withAllModules f = do
    mods <- gets $ M.elems . ircModules :: LB [ModuleRef]
    (`mapM_` mods) $ \(ModuleRef m ref name) -> do
        runReaderT (runModuleT $ f m) (ref, name)
