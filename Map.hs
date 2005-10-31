--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Compatibility code between Data.FiniteMap and Data.Map We use the
-- function names from Data.Map.
--
module Map (
#if __GLASGOW_HASKELL__ >= 604
        module Data.Map,
        addList,
#else
        Map,
        empty,
        insert,
        insertWith,
        delete,
        update,
        lookup,
        toList,
        fromList,
        addList,
        size,
        elems,
        singleton,
        member,
        keys,
        assocs,
        find,
        union,
        (!),

        mapWithKey,
        filterWithKey,
        filter,
        foldWithKey,
        -- null :: Map k a -> Bool
#endif
        mapMaybe,
        insertUpd,
  ) where

import Prelude hiding (lookup, filter)
import Data.Maybe (fromJust, isJust)


#if __GLASGOW_HASKELL__ >= 604
import Data.Map

addList :: (Ord k) => [(k,a)] -> Map k a -> Map k a
addList l m = union (fromList l) m
{-# INLINE addList #-}

#else
--
-- compatibility code for deprecated FiniteMap
--
import Prelude hiding (lookup, filter)
import qualified Data.FiniteMap as FM

type Map = FM.FiniteMap

instance Functor (Map k) where
  fmap = FM.mapFM . const

empty  :: Map k a
empty  = FM.emptyFM

singleton :: k -> a -> Map k a
singleton = FM.unitFM

insert :: Ord k => k -> a -> Map k a -> Map k a
insert = \k e m -> FM.addToFM m k e

-- | This function is pure evil. Avoid it if possible.
--   Otherwise, always remember: The first argument of @f@ is the NEW value 
--   (i.e we already know it), the second argument is the OLD value!
--
--   Grrrrrrrr.
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k e m = FM.addToFM_C (flip f) m k e

delete :: Ord k => k -> Map k a -> Map k a
delete = flip FM.delFromFM

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k m = case lookup k m of
  Nothing -> m
  Just v  -> case f v of
    Nothing -> delete k m
    Just v' -> insert k v' m

lookup :: Ord k => k -> Map k a -> Maybe a
lookup = flip FM.lookupFM

fromList :: Ord k => [(k,a)] -> Map k a
fromList = FM.listToFM

toList :: Map k a -> [(k,a)]
toList = FM.fmToList

size :: Map k a -> Int
size = FM.sizeFM

elems :: Map k a -> [a]
elems = FM.eltsFM

member :: Ord k => k -> Map k a -> Bool
member = FM.elemFM

keys  :: Map k a -> [k]
keys = FM.keysFM

assocs :: Map k a -> [(k, a)]
assocs = FM.fmToList

addList :: (Ord k) => [(k, a)] -> Map k a -> Map k a
addList = flip FM.addListToFM

-- delListFromFM = \fm keys -> foldl delete fm keys

-- Posted by Gracjan Polak on haskell-cafe@
-- note that we want the mapping the other way around.
--
-- deleteList list map = foldl (flip Data.Map.delete) map list
-- insertList asclist map = union map (Data.Map.fromList asclist)
--

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey = FM.mapFM

-- map f m == mapFM (const f)

filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey = FM.filterFM

filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter = filterWithKey . const

foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey = FM.foldFM

infixl 9 !
(!) :: Ord k => Map k a -> k -> a
m ! k = find k m

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
find :: Ord k => k -> Map k a -> a
find k m = case lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x  -> x

union :: Ord k => Map k a -> Map k a -> Map k a
union = FM.plusFM

#endif

-- | Data.Maybe.mapMaybe for Maps
mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = fmap fromJust . filter isJust . fmap f

-- | This makes way more sense than @insertWith@ because we don't need to
--   remember the order of arguments of @f@.
insertUpd :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
insertUpd f = insertWith (\_ -> f)
