{-# LANGUAGE RankNTypes #-}
module Lambdabot.Monad where

import Control.Monad.Trans

data LB a
instance Monad LB
instance MonadIO LB

class Monad m => MonadLB m where lb :: LB a -> m a

lbIO :: MonadLB m => ((forall a. LB a -> IO a) -> IO b) -> m b
