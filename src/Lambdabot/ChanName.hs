module Lambdabot.ChanName
    ( ChanName
    , mkCN
    , getCN
    ) where

import Lambdabot.Nick

import Control.Applicative
import Data.Char

newtype ChanName = ChanName Nick -- always lowercase
  deriving (Eq, Ord)

instance Show ChanName where show (ChanName x) = show x

mkCN :: Nick -> ChanName
mkCN = ChanName . liftA2 Nick nTag (map toLower . nName)

getCN :: ChanName -> Nick
getCN (ChanName n) = n

