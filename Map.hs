--
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Extended map
--
module Map (
        module Data.Map,
        addList,
        mapMaybe,
        insertUpd,
  ) where

import Prelude hiding (lookup, filter)
import Data.Maybe (fromJust, isJust)

import Data.Map

addList :: (Ord k) => [(k,a)] -> Map k a -> Map k a
addList l m = union (fromList l) m
{-# INLINE addList #-}

-- | Data.Maybe.mapMaybe for Maps
mapMaybe :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = fmap fromJust . filter isJust . fmap f

-- | This makes way more sense than @insertWith@ because we don't need to
--   remember the order of arguments of @f@.
insertUpd :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
insertUpd f = insertWith (\_ -> f)
