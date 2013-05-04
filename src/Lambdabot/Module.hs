{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Lambdabot.Module
    ( Module(..)
    , newModule
    
    , ModuleT(..)

    , getRef
    , getModuleName
    , bindModule0
    , bindModule1
    , bindModule2
    ) where

import qualified Lambdabot.Command as Cmd
import Lambdabot.Config
import Lambdabot.Logging
import {-# SOURCE #-} Lambdabot.Monad
import Lambdabot.Util.Serial

import Control.Applicative
import Control.Concurrent (MVar)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Control.Monad.Trans.Control
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

        -- | The commands the module listenes to.
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

--
-- | This transformer encodes the additional information a module might
--   need to access its name or its state.
--
newtype ModuleT st m a = ModuleT { runModuleT :: ReaderT (MVar st, String) m a }
    deriving (Applicative, Functor, Monad, MonadTrans, MonadIO, MonadException, MonadConfig)

instance MonadLogging m => MonadLogging (ModuleT st m) where
    getCurrentLogger = do
        parent <- lift getCurrentLogger
        self   <- getModuleName
        return (parent ++ ["Plugin", self])
    logM a b c = lift (logM a b c)

instance MonadBase b m => MonadBase b (ModuleT st m) where
    liftBase = lift . liftBase

instance MonadTransControl (ModuleT st) where
    newtype StT (ModuleT st) a = StModule {unStModule :: a}
    liftWith f = do
        r <- ModuleT ask
        lift $ f $ \t -> liftM StModule (runReaderT (runModuleT t) r)
    restoreT = lift . liftM unStModule
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ModuleT st m) where
    newtype StM (ModuleT st m) a = StMModule {unStMModule :: ComposeSt (ModuleT st) m a}
    liftBaseWith = defaultLiftBaseWith StMModule
    restoreM     = defaultRestoreM     unStMModule
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

getRef :: Monad m => ModuleT st m (MVar st)
getRef  = ModuleT $ ask >>= return . fst

getModuleName :: Monad m => ModuleT mod m String
getModuleName = ModuleT $ ask >>= return . snd

-- | bind an action to the current module so it can be run from the plain
--   `LB' monad.
bindModule0 :: ModuleT mod LB a -> ModuleT mod LB (LB a)
bindModule0 act = bindModule1 (const act) >>= return . ($ ())

-- | variant of `bindModule0' for monad actions with one argument
bindModule1 :: (a -> ModuleT mod LB b) -> ModuleT mod LB (a -> LB b)
bindModule1 act = ModuleT $
    ask >>= \st -> return (\val -> runReaderT (runModuleT $ act val) st)

-- | variant of `bindModule0' for monad actions with two arguments
bindModule2 :: (a -> b -> ModuleT mod LB c) -> ModuleT mod LB (a -> b -> LB c)
bindModule2 act = bindModule1 (uncurry act) >>= return . curry
