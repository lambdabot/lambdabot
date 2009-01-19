module L where
import Control.Applicative
import Control.Arrow
import Control.Arrow.Operations
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Instances
import Control.Monad.Logic
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.ST (ST, runST, fixST)
import Control.Monad.State
import Control.Monad.Writer
import Control.Parallel
import Control.Parallel.Strategies
import Data.Array
import Data.Bits
import Data.Bool
import Data.Char
import Data.Complex
import Data.Dynamic
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Function hiding ((.))
import Data.Generics hiding (GT)
import Data.Graph
import Data.Int
import Data.Ix
import Data.List hiding ((++),map)
import Data.Maybe
import Data.Monoid
import Data.Number.BigFloat
import Data.Number.CReal
import Data.Number.Dif
import Data.Number.Fixed
import Data.Number.Interval
import Data.Number.Natural
import Data.Number.Symbolic
import Data.Ord
import Data.Ratio
import Data.STRef
import Data.Tree
import Data.Tuple
import Data.Typeable
import Data.Word
import Numeric
import ShowQ
import System.Random
import Test.QuickCheck
import Text.PrettyPrint.HughesPJ hiding (empty)
import Text.Printf
import Text.Regex.Posix
import qualified Control.Arrow.Transformer as AT
import qualified Control.Arrow.Transformer.All as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Foldable
import qualified Data.Generics
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Sequence
import qualified Data.Set as S
import qualified Data.Traversable

import SimpleReflect hiding (var)
import Math.OEIS

describeSequence = fmap description . lookupSequence

newtype Mu f = In { out :: f (Mu f) }

newtype Rec a = InR { outR :: Rec a -> a }

{-# LINE 1 "<local>" #-}
