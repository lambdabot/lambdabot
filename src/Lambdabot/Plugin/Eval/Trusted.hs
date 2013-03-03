{-# LANGUAGE Trustworthy #-}
module Lambdabot.Plugin.Eval.Trusted
    ( module ShowQ
    , module Math.OEIS
    , module Test.QuickCheck
    , module Lambdabot.Plugin.Eval.Trusted
    ) where

import ShowQ
import Math.OEIS
import Test.QuickCheck

describeSequence :: SequenceData -> Maybe String
describeSequence = fmap description . lookupSequence

newtype Mu f = In { out :: f (Mu f) }

newtype Rec a = InR { outR :: Rec a -> a }
