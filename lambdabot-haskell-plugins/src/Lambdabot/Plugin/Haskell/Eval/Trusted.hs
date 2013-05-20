{-# LANGUAGE Trustworthy #-}
module Lambdabot.Plugin.Haskell.Eval.Trusted
    ( module Math.OEIS
    , module Test.QuickCheck
    , module Lambdabot.Plugin.Haskell.Check.ShowQ
    , module Lambdabot.Plugin.Haskell.Eval.Trusted
    ) where

import Math.OEIS
import Lambdabot.Plugin.Haskell.Check.ShowQ
import Test.QuickCheck

describeSequence :: SequenceData -> Maybe String
describeSequence = fmap description . lookupSequence

newtype Mu f = In { out :: f (Mu f) }

newtype Rec a = InR { outR :: Rec a -> a }
