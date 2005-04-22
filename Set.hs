--
-- | Set compatibility layer
--
module Set  ( 
#if __GLASGOW_HASKELL__ > 602
    module Data.Set
#else
    Set,
    member,
    elems,
    delete,
    empty,
    singleton,
    null,
    unions,
    fromList,
    toList,
    union,
    insert,
    size
#endif
  ) where

import Data.Set
#if __GLASGOW_HASKELL__ <= 602
import Prelude (Ord,Bool,Int,flip)

member :: Ord a => a -> Set a -> Bool
member = elementOf

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

delete :: Ord a => a -> Set a -> Set a
delete = flip delFromSet

size :: Set a -> Int
size = cardinality
#endif
