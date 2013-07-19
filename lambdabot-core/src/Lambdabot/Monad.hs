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
    , OutputFilter
    , Server
    , IRCRWState(..)
    , initRwState
    
    , LB
    , runLB
    
    , MonadLB(..)
    
    , registerModule
    , registerCommands
    , registerCallback
    , registerOutputFilter
    , unregisterModule
    
    , registerServer
    , unregisterServer
    , send
    , received
    
    , applyOutputFilters
    
    , inModuleNamed
    , inModuleWithID
    
    , withCommand
    
    , listModules
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
import           Lambdabot.Util

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Exception.Lifted as E (catch)
import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.IORef
import Data.Some
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
    , ircConfig         :: D.DMap Config Identity
    }

-- | Default ro state
initRoState :: [D.DSum Config Identity] -> IO IRCRState
initRoState configuration = do
    quitMVar     <- newEmptyMVar
    initDoneMVar <- newEmptyMVar
    
    let mergeConfig' k (Identity x) (Identity y) = Identity (mergeConfig k y x)
    
    return IRCRState 
        { ircQuitMVar       = quitMVar
        , ircInitDoneMVar   = initDoneMVar
        , ircConfig         = D.fromListWithKey mergeConfig' configuration
        }

reportInitDone :: MonadIO m => IRCRState -> m ()
reportInitDone = io . flip putMVar () . ircInitDoneMVar

askLB :: MonadLB m => (IRCRState -> a) -> m a
askLB f  = lb . LB $ asks (f . fst)

waitForInit :: MonadLB m => m ()
waitForInit = readMVar =<< askLB ircInitDoneMVar

waitForQuit :: MonadLB m => m ()
waitForQuit = readMVar =<< askLB ircQuitMVar

type Callback     st = IrcMessage -> ModuleT st LB ()
type OutputFilter st = Nick -> [String] -> ModuleT st LB [String]
type Server       st = IrcMessage -> ModuleT st LB ()

newtype CallbackRef     st = CallbackRef     (Callback st)
newtype CommandRef      st = CommandRef      (Command (ModuleT st LB))
newtype OutputFilterRef st = OutputFilterRef (OutputFilter st)
newtype ServerRef       st = ServerRef       (Server st)

-- | Global read\/write state.
data IRCRWState = IRCRWState
    { ircServerMap       :: M.Map String (DSum ModuleID ServerRef)
    , ircPrivilegedUsers :: S.Set Nick
    , ircIgnoredUsers    :: S.Set Nick
    
    , ircChannels        :: M.Map ChanName String
    -- ^ maps channel names to topics
    , ircPersists        :: M.Map String Bool
    -- ^ lists servers to which to reconnect on failure (one-time or always)
    
    , ircModulesByName   :: M.Map String (Some ModuleInfo)
    , ircModulesByID     :: D.DMap ModuleID ModuleInfo
    , ircCallbacks       :: M.Map String (D.DMap ModuleID CallbackRef)
    , ircOutputFilters   :: [DSum ModuleID OutputFilterRef]
    -- ^ Output filters, invoked from right to left
    
    , ircCommands        :: M.Map String (DSum ModuleID CommandRef)
    }

-- | Default rw state
initRwState :: IRCRWState
initRwState = IRCRWState
    { ircPrivilegedUsers = S.empty
    , ircIgnoredUsers    = S.empty
    , ircChannels        = M.empty
    , ircPersists        = M.empty
    , ircModulesByName   = M.empty
    , ircModulesByID     = D.empty
    , ircServerMap       = M.empty
    , ircCallbacks       = M.empty
    , ircOutputFilters   = []
    , ircCommands        = M.empty
    }

-- ---------------------------------------------------------------------
--
-- The LB (LambdaBot) monad
--

-- | The IRC Monad. The reader transformer holds information about the
--   connection to the IRC server.
--
-- instances Monad, Functor, MonadIO, MonadState, MonadError

newtype LB a = LB { unLB :: ReaderT (IRCRState, IORef IRCRWState) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadException)

runLB :: LB a -> (IRCRState, IORef IRCRWState) -> IO a
runLB = runReaderT . unLB

instance MonadBase IO LB where
    liftBase = LB . liftBase

instance MonadBaseControl IO LB where
    type StM LB a = StM (ReaderT (IRCRState,IORef IRCRWState) IO) a
    liftBaseWith action = LB (liftBaseWith (\run -> action (run . unLB)))
    restoreM = LB . restoreM

class (MonadIO m, MonadBaseControl IO m, MonadConfig m, MonadLogging m, Applicative m) => MonadLB m where
    lb :: LB a -> m a

instance MonadLB LB where lb = id
instance MonadLB m => MonadLB (ModuleT st m) where lb = lift . lb
instance MonadLB m => MonadLB (Cmd m)        where lb = lift . lb

instance MonadState IRCRWState LB where
    state f = LB $ do
        ref <- asks snd
        lift . atomicModifyIORef ref $ \s -> 
            let (s', x) = f s
             in seq s' (x, s')

instance MonadConfig LB where
    getConfig k = liftM (maybe (getConfigDefault k) runIdentity . D.lookup k) (lb (askLB ircConfig))

instance MonadLogging LB where
    getCurrentLogger = getConfig lbRootLoggerPath
    logM a b c = io (logM a b c)

---------------
-- state management (registering/unregistering various things)

registerModule :: String -> Module st -> st -> LB (ModuleInfo st)
registerModule mName m mState = do
    mTag    <- io newModuleID
    mInfo   <- ModuleInfo mName mTag m <$> newMVar mState
    
    modify $ \s -> s
        { ircModulesByName  = M.insert mName (This mInfo) (ircModulesByName s)
        , ircModulesByID    = D.insert mTag        mInfo  (ircModulesByID   s)
        }
    
    return mInfo

registerCommands :: [Command (ModuleT st LB)] -> ModuleT st LB ()
registerCommands cmds = do
    mTag <- asks moduleID
    let taggedCmds = 
            [ (cName, mTag :=> CommandRef cmd)
            | cmd   <- cmds
            , cName <- cmdNames cmd
            ]
    
    lift $ modify $ \s -> s
        { ircCommands = M.union (M.fromList taggedCmds) (ircCommands s)
        }

registerCallback :: String -> Callback st -> ModuleT st LB ()
registerCallback str f = do
    mTag <- asks moduleID
    
    lift . modify $ \s -> s
        { ircCallbacks = M.insertWith D.union str
            (D.singleton mTag (CallbackRef f))
            (ircCallbacks s)
        }

registerOutputFilter :: OutputFilter st -> ModuleT st LB ()
registerOutputFilter f = do
    mTag <- asks moduleID
    lift . modify $ \s -> s
        { ircOutputFilters = (mTag :=> OutputFilterRef f) : ircOutputFilters s
        }

unregisterModule :: String -> LB ()
unregisterModule mName = maybe (return ()) warningM <=< state $ \s -> 
    case M.lookup mName (ircModulesByName s) of
        Nothing                 -> (Just $ "Tried to unregister module that wasn't registered: " ++ show mName, s)
        Just (This modInfo)     ->
            let mTag = moduleID modInfo
                
                notThisTag :: DSum ModuleID f -> Bool
                notThisTag (tag :=> _) = This tag /= This mTag
                s' = s
                    { ircModulesByName  = M.delete mName        (ircModulesByName s)
                    , ircModulesByID    = D.delete mTag         (ircModulesByID   s)
                    , ircCommands       = M.filter notThisTag   (ircCommands      s)
                    , ircCallbacks      = M.map (D.delete mTag) (ircCallbacks     s)
                    , ircServerMap      = M.filter notThisTag   (ircServerMap     s)
                    , ircOutputFilters  =   filter notThisTag   (ircOutputFilters s)
                    }
             in (Nothing, s')

-- The virtual chat system.
--
-- The virtual chat system sits between the chat drivers and the rest of
-- Lambdabot.  It provides a mapping between the String server "tags" and
-- functions which are able to handle sending messages.
--
-- When a message is recieved, the chat module is expected to call
-- `Lambdabot.Main.received'.  This is not ideal.

registerServer :: String -> Server st -> ModuleT st LB ()
registerServer sName sendf = do
    mTag <- asks moduleID
    maybe (return ()) fail <=< lb . state $ \s ->
        case M.lookup sName (ircServerMap s) of
            Just _  -> (Just $ "attempted to create two servers named " ++ sName, s)
            Nothing -> 
                let s' = s { ircServerMap = M.insert sName (mTag :=> ServerRef sendf) (ircServerMap s)}
                 in (Nothing, s')

-- TODO: fix race condition
unregisterServer :: String -> ModuleT mod LB ()
unregisterServer tag = lb $ do
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

withUEHandler :: LB () -> LB ()
withUEHandler f = do
    handler <- getConfig uncaughtExceptionHandler
    E.catch f (io . handler)

send :: IrcMessage -> LB ()
send msg = do
    s <- gets ircServerMap
    let bogus = warningM $ "sending message to bogus server: " ++ show msg
    case M.lookup (Msg.server msg) s of
        Just (mTag :=> ServerRef sendf) -> 
            withUEHandler (inModuleWithID mTag bogus (sendf msg))
        Nothing -> bogus

received :: IrcMessage -> LB ()
received msg = do
    s       <- get
    case M.lookup (ircMsgCommand msg) (ircCallbacks s) of
        Just cbs -> forM_ (D.toList cbs) $ \(tag :=> CallbackRef cb) ->
            withUEHandler (inModuleWithID tag (return ()) (cb msg))
        _        -> return ()

applyOutputFilter :: Nick -> DSum ModuleID OutputFilterRef -> [String] -> LB [String]
applyOutputFilter who (mTag :=> OutputFilterRef f) msg =
    inModuleWithID mTag (return msg) (f who msg)

applyOutputFilters :: Nick -> String -> LB [String]
applyOutputFilters who msg = do
    filters   <- gets ircOutputFilters
    foldr (\a x -> applyOutputFilter who a =<< x) ((return . lines) msg) filters

------------------------------------------------------------------------
-- Module handling

-- | Interpret an expression in the context of a module.
inModuleNamed :: String -> LB a -> (forall st. ModuleT st LB a) -> LB a
inModuleNamed name nothing just = do
    mbMod <- gets (M.lookup name . ircModulesByName)
    case mbMod of
        Nothing             -> nothing
        Just (This modInfo) -> runModuleT just modInfo

inModuleWithID :: ModuleID st -> LB a -> (ModuleT st LB a) -> LB a
inModuleWithID tag nothing just = do
    mbMod <- gets (D.lookup tag . ircModulesByID )
    case mbMod of
        Nothing         -> nothing
        Just modInfo    -> runModuleT just modInfo

withCommand :: String -> LB a -> (forall st. Command (ModuleT st LB) -> ModuleT st LB a) -> LB a
withCommand cmdname def f = do
    mbCmd <- gets (M.lookup cmdname . ircCommands)
    case mbCmd of
        Just (tag :=> CommandRef cmd)   -> inModuleWithID tag def (f cmd)
        _                               -> def

listModules :: LB [String]
listModules = gets (M.keys . ircModulesByName)

-- | Interpret a function in the context of all modules
withAllModules :: (forall st. Module st -> ModuleT st LB a) -> LB ()
withAllModules f = do
    mods <- gets $ M.elems . ircModulesByName
    (`mapM_` mods) $ \(This modInfo) ->
        runModuleT (f (theModule modInfo)) modInfo
