{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Lambdabot.Module
    ( MODULE(..), Module(..)
    , ModuleT(..)
    
    , ModuleRef(..), CommandRef(..)
    
    , getRef, getName, bindModule0, bindModule1, bindModule2
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
class Module m where
    type ModuleState m
    type ModuleState m = ()
    
    -- | If the module wants its state to be saved, this function should
    --   return a Serial.
    --
    --   The default implementation returns Nothing.
    moduleSerialize :: m -> Maybe (Serial (ModuleState m))

    -- | If the module maintains state, this method specifies the default state
    --   (for example in case the state can't be read from a state).
    --
    --   The default implementation returns an error and assumes the state is
    --   never accessed.
    moduleDefState  :: m -> LB (ModuleState m)

    -- | Is the module sticky? Sticky modules (as well as static ones) can't be
    --   unloaded. By default, modules are not sticky.
    moduleSticky    :: m -> Bool

    -- | The commands the module listenes to.
    moduleCmds      :: ModuleT m LB [Cmd.Command (ModuleT m LB)]

    -- | Initialize the module. The default implementation does nothing.
    moduleInit      :: ModuleT m LB ()

    -- | Finalize the module. The default implementation does nothing.
    moduleExit      :: ModuleT m LB ()

    -- | Process contextual input. A plugin that implements 'contextual'
    -- is able to respond to text not part of a normal command.
    contextual
        :: m                                -- ^ phantom     (required)
        -> String                           -- ^ the text
        -> Cmd.Cmd (ModuleT m LB) ()        -- ^ the action

------------------------------------------------------------------------

    contextual _ _ = return ()

    moduleCmds         = return []
    moduleExit         = return ()
    moduleInit         = return ()
    moduleSticky _     = False
    moduleSerialize _  = Nothing
    moduleDefState  _  = return $ error "state not initialized"

-- | An existential type holding a module, used to represent modules on
-- the value level, for manipluation at runtime by the dynamic linker.
data MODULE = forall m. Module m => MODULE m

data ModuleRef = forall m s. 
    Module m => ModuleRef m (MVar (ModuleState m)) String

data CommandRef = forall m s.
    Module m => CommandRef m (MVar (ModuleState m)) (Cmd.Command (ModuleT m LB)) String

--
-- | This transformer encodes the additional information a module might
--   need to access its name or its state.
--
newtype ModuleT mod m a = ModuleT { moduleT :: ReaderT (MVar (ModuleState mod), String) m a }
    deriving (Functor, Monad, MonadTrans, MonadIO, MonadError e, MonadState t)

instance MonadLB m => MonadLB      (ModuleT mod m) where lb = lift . lb
instance MonadLB m => MonadLBState (ModuleT mod m) where
    type LBState (ModuleT mod m) = ModuleState mod
    withMS f = do
        ref <- getRef
        lbIO $ \conv -> withMWriter ref $ \x writer ->
            conv $ f x (liftIO . writer)


getRef :: Monad m => ModuleT mod m (MVar (ModuleState mod))
getRef  = ModuleT $ ask >>= return . fst

getName :: Monad m => ModuleT mod m String
getName = ModuleT $ ask >>= return . snd

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
