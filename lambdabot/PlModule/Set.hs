{-# OPTIONS -cpp -fno-implicit-prelude #-}
module PlModule.Set  ( 
    Set,
    elems,
    empty,
    singleton,
    null,
    unions,
    fromList,
    toList,
    union,
    insert,
#if __GLASGOW_HASKELL__ > 602
    mapMonotonic,
#endif
    size
  ) where


#if __GLASGOW_HASKELL__ > 602
import qualified Data.Set as S
import Prelude (Ord,Bool,Int)
type Set = S.Set

elems :: Set a -> [a]
elems = S.elems

empty :: Set a
empty = S.empty

singleton :: a -> Set a
singleton = S.singleton

null :: Set a -> Bool
null = S.null

unions :: (Ord a) => [Set a] -> Set a
unions = S.unions

fromList :: (Ord a) => [a] -> Set a
fromList = S.fromList

toList :: Set a -> [a]
toList = S.toList

union :: (Ord a) => Set a -> Set a -> Set a
union = S.union

insert :: (Ord a) => a -> Set a -> Set a
insert = S.insert

size :: Set a -> Int
size = S.size

mapMonotonic :: (a -> b) -> Set a -> Set b
mapMonotonic = S.mapMonotonic

#else
import Prelude (Ord,Bool,Int,flip)
import Data.Set

elems :: Set a -> [a]
elems = setToList

empty :: Set a
empty = emptySet

singleton :: a -> Set a
singleton = unitSet

null :: Set a -> Bool
null = isEmptySet

unions :: (Ord a) => [Set a] -> Set a
unions = unionManySets

fromList :: (Ord a) => [a] -> Set a
fromList = mkSet

toList :: Set a -> [a]
toList = setToList

--union :: (Ord a) => Set a -> Set a -> Set a
--union = union

insert :: (Ord a) => a -> Set a -> Set a
insert = flip addToSet

size :: Set a -> Int
size = cardinality
#endif
