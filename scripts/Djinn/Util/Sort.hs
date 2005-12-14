{- Copyright (c) 2001,2002 Galois Connections, Inc.
 -}
{- |
 
  Module      :  Util.Sort
  Copyright   :  (c) Galois Connections 2001, 2002

  Maintainer      : lib@galois.com
  Stability       : 
  Portability     : 
  
  Extra sorting functions - copied from GHC compiler sources (util\/Util.lhs)
-}
module Util.Sort where

sortLt :: (a -> a -> Bool) 		-- Less-than predicate
       -> [a] 				-- Input list
       -> [a]				-- Result list

sortLt lt l = qsort lt l []

-- qsort is stable and does not concatenate.
qsort :: (a -> a -> Bool) -- Less-than predicate
      -> [a]		  -- xs, Input list
      -> [a]              -- r,  Concatenate this list to the sorted input list
      -> [a]		  -- Result = sort xs ++ r

qsort _  []     r = r
qsort _  [x]    r = x:r
qsort lt (x:xs) r = qpart lt x xs [] [] r

-- qpart partitions and sorts the sublists
-- rlt contains things less than x,
-- rge contains the ones greater than or equal to x.
-- Both have equal elements reversed with respect to the original list.

qpart :: (a -> a -> Bool) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart lt x [] rlt rge r =
    -- rlt and rge are in reverse order and must be sorted with an
    -- anti-stable sorting
    rqsort lt rlt (x : rqsort lt rge r)

qpart lt x (y:ys) rlt rge r =
    if lt y x then
	-- y < x
	qpart lt x ys (y:rlt) rge r
    else
	-- y >= x
	qpart lt x ys rlt (y:rge) r

-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
rqsort :: (a -> a -> Bool)    -- Less-than predicate
       -> [a]		      -- xs, Input list
       -> [a]		      -- r,  Concatenate this list to the sorted input
       -> [a]		      -- Result = sort xs ++ r
rqsort _ []      r = r
rqsort _ [x]     r = x:r
rqsort lt (x:xs) r = rqpart lt x xs [] [] r

rqpart :: (a -> a -> Bool) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart lt x [] rle rgt r =
    qsort lt rle (x : qsort lt rgt r)

rqpart lt x (y:ys) rle rgt r =
    if lt x y then
	-- y > x
	rqpart lt x ys rle (y:rgt) r
    else
	-- y <= x
	rqpart lt x ys (y:rle) rgt r

sortLe :: (a->a->Bool) -> [a] -> [a]
sortLe le = generalNaturalMergeSort le

mergeSort, naturalMergeSort :: Ord a => [a] -> [a]
mergeSort = generalMergeSort (<=)
naturalMergeSort = generalNaturalMergeSort (<=)

generalMergeSort :: (a->a->Bool) -> [a] -> [a]
generalMergeSort _ [] = []
generalMergeSort p xs = (balancedFold (generalMerge p) . map (: [])) xs

generalMerge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
generalMerge _ xs [] = xs
generalMerge _ [] ys = ys
generalMerge p (x:xs) (y:ys) | x `p` y   = x : generalMerge p xs (y:ys)
			     | otherwise = y : generalMerge p (x:xs) ys

balancedFold :: (a -> a -> a) -> [a] -> a
balancedFold _ [] = error "Util.Sort.balancedFold: can't reduce an empty list"
balancedFold _ [x] = x
balancedFold f l  = balancedFold f (balancedFold' f l)

balancedFold' :: (a -> a -> a) -> [a] -> [a]
balancedFold' f (x:y:xs) = f x y : balancedFold' f xs
balancedFold' _ xs = xs

generalNaturalMergeSort :: (a -> a -> Bool) -> [a] -> [a]
generalNaturalMergeSort _   [] = []
generalNaturalMergeSort prd rs = (balancedFold (generalMerge prd) . group prd) rs
  where
   --group :: (a -> a -> Bool) -> [a] -> [[a]]
   group _ []     = []
   group p (l:ls) = group' ls l l (l:)
    where
     group' []     _     _     s  = [s []]
     group' (x:xs) x_min x_max s 
	| not (x `p` x_max) = group' xs x_min x (s . (x :)) 
	| x `p` x_min       = group' xs x x_max ((x :) . s) 
	| otherwise         = s [] : group' xs x x (x :) 
