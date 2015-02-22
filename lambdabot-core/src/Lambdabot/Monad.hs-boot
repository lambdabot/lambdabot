{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ > 706
{-# LANGUAGE RoleAnnotations #-}
#endif
module Lambdabot.Monad where

import Control.Applicative

#if __GLASGOW_HASKELL__ > 706
type role LB nominal
#endif
data LB a
instance Applicative LB
instance Functor LB
instance Monad LB
