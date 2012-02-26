{-# LANGUAGE Trustworthy #-}
module Trusted
    ( module ShowQ
    , module Math.OEIS
    , module Test.QuickCheck
    , module Trusted
    ) where

import ShowQ
import Math.OEIS
import Test.QuickCheck

describeSequence = fmap description . lookupSequence

newtype Mu f = In { out :: f (Mu f) }

newtype Rec a = InR { outR :: Rec a -> a }
