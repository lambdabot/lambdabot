{-# OPTIONS -cpp -fglasgow-exts #-}
-- Ambiguous type variables

-- 	$Id: Util.hs,v 1.10 2003/07/31 19:13:15 eleganesh Exp $

module Util (
     join,
     split,
     breakOnGlue,
     snoc,
     split_first_word,
     debugStr,
     debugStrLn,
     Accessor (..),
     readFM, writeFM, deleteFM,
     lookupSet, insertSet, deleteSet,
     getRandItem, stdGetRandItem
) where

import BotConfig                (getVerbose)
import Map                      (Map)
import qualified Map as M       (lookup, insert, delete)

import Data.List                (intersperse, isPrefixOf)
import Data.Set                 (elementOf, addToSet, delFromSet, Set)
import Data.Char                (isSpace)
import Data.Typeable
import Control.Monad.State      (MonadIO(..))

import System.Random hiding (split)

------------------------------------------------------------------------

-- TODO: rename join, clashes with Monad.join

finiteMapTyCon :: TyCon
finiteMapTyCon = mkTyCon "Map"

instance (Typeable key, Typeable elt) => Typeable (Map key elt) where
    typeOf _ = mkMyTy finiteMapTyCon [typeOf (undefined :: key),
			    	      typeOf (undefined :: elt)]
        where
#if __GLASGOW_HASKELL__ >= 603
         mkMyTy = mkTyConApp
#else
         mkMyTy = mkAppTy
#endif

-- | Join lists with the given glue elements. Example:
--
-- > join ", " ["one","two","three"] ===> "one, two, three"


join :: [a]   -- ^ Glue to join with
     -> [[a]] -- ^ Elements to glue together
     -> [a]   -- ^ Result: glued-together list

join glue xs = (concat . intersperse glue) xs


-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"]

split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces

split glue xs = split' xs
    where
    split' [] = []
    split' xs' = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs'
    dropGlue = drop (length glue)


-- | Break off the first piece of a list held together by glue,
--   leaving the glue attached to the remainder of the list.  Example:
--   Like break, but works with a [a] match.
--
-- > breakOnGlue ", " "one, two, three" ===> ("one", ", two, three")

breakOnGlue :: (Eq a) => [a] -- ^ Glue that holds pieces together
            -> [a]           -- ^ List from which to break off a piece
            -> ([a],[a])     -- ^ Result: (first piece, glue ++ rest of list)

breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest') 
        where (piece, rest') = breakOnGlue glue xs
{-# INLINE breakOnGlue #-}

-- | Reverse cons. Add an element to the back of a list. Example:
--
-- > snoc 3 [2, 1] ===> [2, 1, 3]
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- | Break a String into it's first word, and the rest of the string. Example:
--
-- > split_first_word "A fine day" ===> ("A", "fine day)
split_first_word :: String -> (String, String)
split_first_word xs = (w, dropWhile isSpace xs')
  where (w, xs') = break isSpace xs

-- refactor, might be good for logging to file later
debugStr :: (MonadIO m) => String -> m ()
debugStr x = do verbose <- getVerbose
                if verbose then liftIO (putStr x) else return ()

debugStrLn :: (MonadIO m) => [Char] -> m ()
debugStrLn x = debugStr ( x ++ "\n" )

data Accessor m s = Accessor { reader :: m s, writer :: s -> m () }

readFM :: (Monad m,Ord k) => Accessor m (Map k e) -> k -> m (Maybe e)
readFM a k = do fm <- reader a
                return $ M.lookup k fm

writeFM :: (Monad m,Ord k) => Accessor m (Map k e) -> k -> e -> m ()
writeFM a k e = do fm <- reader a
                   writer a $ M.insert k e fm

deleteFM :: (Monad m,Ord k) => Accessor m (Map k e) -> k -> m ()
deleteFM a k = do fm <- reader a
                  writer a $ M.delete k fm

lookupSet :: (Monad m,Ord e) => Accessor m (Set e) -> e -> m Bool
lookupSet a e = do set <- reader a
                   return $ elementOf e set

insertSet :: (Monad m,Ord e) => Accessor m (Set e) -> e -> m ()
insertSet a e = do set <- reader a
                   writer a $ addToSet set e

deleteSet :: (Monad m,Ord e) => Accessor m (Set e) -> e -> m ()
deleteSet a e = do set <- reader a
                   writer a $ delFromSet set e

------------------------------------------------------------------------

-- | 'getRandItem' takes as input a list and a random number generator. It
--   then returns a random element from the list, paired with the altered
--   state of the RNG
getRandItem :: (RandomGen g) => [a] -> g -> (a, g)
getRandItem mylist rng = (mylist !! index,newRng)
                         where
                         llen = length mylist
                         (index, newRng) = randomR (0,llen - 1) rng

-- | 'stdGetRandItem' is the specialization of 'getRandItem' to the standard
--   RNG embedded within the IO monad. The advantage of using this is that
--   you use the Operating Systems provided RNG instead of rolling your own
--   and the state of the RNG is hidden, so one don't need to pass it
--   explicitly.
stdGetRandItem :: [a] -> IO a
stdGetRandItem lst = getStdRandom $ getRandItem lst