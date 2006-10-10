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
epsilon = last $ takeWhile (\x -> 1 + x /= 1) $ iterate (/2) 1
wrap [a,b,c] = (a,b,c)
f x = r where r = x + 1
fib' 0 = (0,1)
fib' n = if n `mod` 2 == 0 then (let { (a,b) = fib' (n `div` 2 - 1) ; c = a + b ; c2 = c*c } in (c2 - a*a, c2 + b*b)) else (let { (a,b) = fib' ((n-1) `div` 2) ; c = a + b; a2 = a*a } in (b*b + a2, c*c - a2))
fib n = fst (fib' n)
