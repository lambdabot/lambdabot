-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
-- Copyright date and holder unknown.
module Lambdabot.Plugin.Haskell.Check.ShowQ (myquickcheck) where

import Test.QuickCheck.Safe (STestable, quickCheck, inventQCGen)

myquickcheck :: STestable prop => prop -> String
myquickcheck prop = quickCheck (inventQCGen prop) prop
