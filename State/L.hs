module L where

import Prelude
import Char
import List
import Maybe
import Numeric
import Random
import Data.Array
import Data.Complex
import Data.Generics
import Data.Bits
import Data.Bool
import Data.Char
import Data.Dynamic
import Data.Either
import Data.Graph
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Tree
import Data.Tuple
import Data.Typeable
import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as I
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Fix
import Control.Arrow
import Text.Printf
import Test.QuickCheck
import ShowQ

{-# LINE 1 "<local>" #-}
splitBy p = unfoldr (\xs -> case (break p xs) of ([],_) -> Nothing; (xs,[]) -> Just (xs, []); (xs,ys) -> Just (xs, tail ys))
on f g a b = (f a) `g` (f b)
state = flip execState
