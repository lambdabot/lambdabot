{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lambdabot.Module
    ( Module(..)
    , newModule
    
    , ModuleID
    , newModuleID
    
    , ModuleInfo(..)
    , ModuleT
    , runModuleT
    ) where

import qualified Lambdabot.Command as Cmd
import Lambdabot.Config
import Lambdabot.Logging
import {-# SOURCE #-} Lambdabot.Monad
import Lambdabot.Util.Serial

import Control.Applicative
import Control.Concurrent (MVar)
import Control.Monad
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail
import Control.Monad.Base
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Control.Monad.Trans.Control
import Data.Unique.Tag
import System.Console.Haskeline.MonadException (MonadException)

------------------------------------------------------------------------

-- | The Module type class.
data Module st = Module {
        -- | If the module wants its state to be saved, this function should
        --   return a Serial.
        --
        --   The default implementation returns Nothing.
        moduleSerialize :: !(Maybe (Serial st)),

        -- | If the module maintains state, this method specifies the default state
        --   (for example in case the state can't be read from a state).
        --
        --   The default implementation returns an error and assumes the state is
        --   never accessed.
        moduleDefState  :: !(LB st),

        -- | Is the module sticky? Sticky modules (as well as static ones) can't be
        --   unloaded. By default, modules are not sticky.
        moduleSticky    :: !Bool,

        -- | The commands the module listens to.
        moduleCmds      :: !(ModuleT st LB [Cmd.Command (ModuleT st LB)]),

        -- | Initialize the module. The default implementation does nothing.
        moduleInit      :: !(ModuleT st LB ()),

        -- | Finalize the module. The default implementation does nothing.
        moduleExit      :: !(ModuleT st LB ()),

        -- | Process contextual input. A plugin that implements 'contextual'
        -- is able to respond to text not part of a normal command.
        contextual
            :: !(String                           --  the text
             -> Cmd.Cmd (ModuleT st LB) ())       -- ^ the action
    }

------------------------------------------------------------------------

newModule :: Module st
newModule = Module
    { contextual         = \_ -> return ()
    , moduleCmds         = return []
    , moduleExit         = return ()
    , moduleInit         = return ()
    , moduleSticky       = False
    , moduleSerialize    = Nothing
    , moduleDefState     = return $ error "state not initialized"
    }

newtype ModuleID st = ModuleID (Tag RealWorld st)
    deriving (GEq, GCompare)

newModuleID :: IO (ModuleID st)
newModuleID = ModuleID <$> newTag

-- |Info about a running module.
data ModuleInfo st = ModuleInfo
    { moduleName   :: !String
    , moduleID     :: !(ModuleID st)
    , theModule    :: !(Module st)
    , moduleState  :: !(MVar st)
    }

-- | This transformer encodes the additional information a module might
--   need to access its name or its state.
newtype ModuleT st m a = ModuleT { unModuleT :: ReaderT (ModuleInfo st) m a }
    deriving (Applicative, Functor, Monad, MonadReader (ModuleInfo st), 
        MonadTrans, MonadIO, MonadException, MonadConfig, MonadFail)

runModuleT :: ModuleT st m a -> ModuleInfo st -> m a
runModuleT = runReaderT . unModuleT

instance MonadLogging m => MonadLogging (ModuleT st m) where
    getCurrentLogger = do
        parent <- lift getCurrentLogger
        self   <- asks moduleName
        return (parent ++ ["Plugin", self])
    logM a b c = lift (logM a b c)

instance MonadBase b m => MonadBase b (ModuleT st m) where
    liftBase = lift . liftBase

instance MonadTransControl (ModuleT st) where
    type StT (ModuleT st) a = a
    liftWith f = do
        r <- ModuleT ask
        lift $ f $ \t -> runModuleT t r
    restoreT = lift
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ModuleT st m) where
    type StM (ModuleT st m) a = ComposeSt (ModuleT st) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}
