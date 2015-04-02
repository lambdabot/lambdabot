{-# LANGUAGE Trustworthy #-}
module Lambdabot.Plugin.Haskell.Eval.Trusted
    ( module Test.QuickCheck.Safe
    , module Lambdabot.Plugin.Haskell.Check.ShowQ
    , module Lambdabot.Plugin.Haskell.Eval.Trusted
    , module GHC.Exts, Constraint
    ) where

import Lambdabot.Plugin.Haskell.Check.ShowQ
import Test.QuickCheck.Safe
import GHC.Exts (Constraint, IsList (..), IsString (..))

newtype Mu f = In { out :: f (Mu f) }

newtype Rec a = InR { outR :: Rec a -> a }
