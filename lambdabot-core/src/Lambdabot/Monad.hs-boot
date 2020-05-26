{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
module Lambdabot.Monad where

import Control.Applicative

type role LB nominal
data LB a
instance Applicative LB
instance Functor LB
instance Monad LB
