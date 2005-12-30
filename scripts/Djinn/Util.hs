module Util(mapFst, mapSnd, insert, makeSet) where
import List(sort)

mapFst :: (a->b) -> [(a,c)] -> [(b, c)]
mapFst f xys = [ (f x, y) | (x, y) <- xys ]

mapSnd :: (a->b) -> [(c, a)] -> [(c, b)]
mapSnd f xys = [ (x, f y) | (x, y) <- xys ]

insert :: (Ord a) => a -> [a] -> [a]
insert a [] = [a]
insert a as@(a':_) | a < a' = a : as
insert a (a':as) = a' : insert a as

makeSet :: (Ord a) => [a] -> [a]
makeSet = remDup . sort
  where remDup [] = []
	remDup [x] = [x]
	remDup (x : xs@(x' : _)) | x == x' = remDup xs
	remDup (x : xs) = x : remDup xs
