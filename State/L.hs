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
fac n = product [1..n]
fib3' a b c d = if a `div` c == b `div` d then a `div` c else fib3' (a+b) a (c+d) c
fib3 0 = 0
fib3 1 = 1
fib3 2 = 1
fib3 3 = 2
fib3 k = let { (q,r) = k `divMod` 2; a = fib3 q; b = fib3' (4*a+1) (2*a+1) 2 2 } in if r == 0 then 2*a*b - a*a else a*a + b*b
test = 4
snd3 (_,x,_) = x
seqZip = ((tail >>=) . zipWith)
sieve (p:ns) = p : sieve (filter (\n -> n `mod` p /= 0) ns)
posRandom = let f x = (do (r, g') <- gets (randomR (0,3)); put g'; case r of 0 -> return x; 1 -> f (x * 2); _ -> f (x * 2 + 1)) in f 0 :: State StdGen Integer
x = "x"
