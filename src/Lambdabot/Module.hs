{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Lambdabot.Module
    ( MODULE(..), Module(..), newModule
    , ModuleT(..)
    
    , ModuleRef(..), CommandRef(..)
    
    , getRef, getModuleName, bindModule0, bindModule1, bindModule2
    ) where

import qualified Lambdabot.Command as Cmd

import {-# SOURCE #-} Lambdabot.Monad (LB, MonadLB(..), lbIO)
import Lambdabot.Serial
import Lambdabot.State
import Lambdabot.Util (withMWriter)

import Control.Concurrent (MVar)
import Control.Monad.Error (MonadError(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))

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
            :: !(String                           -- ^ the text
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

-- | An existential type holding a module, used to represent modules on
-- the value level, for manipluation at runtime by the dynamic linker.
data MODULE = forall st. MODULE !(Module st)

data ModuleRef = forall st. 
    ModuleRef (Module st) (MVar st) String

data CommandRef = forall st.
    CommandRef (Module st) (MVar st) (Cmd.Command (ModuleT st LB)) String

--
-- | This transformer encodes the additional information a module might
--   need to access its name or its state.
--
newtype ModuleT st m a = ModuleT { moduleT :: ReaderT (MVar st, String) m a }
    deriving (Functor, Monad, MonadTrans, MonadIO, MonadError e, MonadState t)

instance MonadLB m => MonadLB      (ModuleT st m) where lb = lift . lb
instance MonadLB m => MonadLBState (ModuleT st m) where
    type LBState (ModuleT st m) = st
    withMS f = do
        ref <- getRef
        lbIO $ \conv -> withMWriter ref $ \x writer ->
            conv $ f x (liftIO . writer)


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
    ask >>= \st -> return (\val -> runReaderT (moduleT $ act val) st)

-- | variant of `bindModule0' for monad actions with two arguments
bindModule2 :: (a -> b -> ModuleT mod LB c) -> ModuleT mod LB (a -> b -> LB c)
bindModule2 act = bindModule1 (uncurry act) >>= return . curry
