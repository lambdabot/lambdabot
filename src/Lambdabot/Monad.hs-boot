{-# LANGUAGE RankNTypes #-}
module Lambdabot.Monad where

import Control.Monad.Trans

data LB a
instance Monad LB
instance MonadIO LB
