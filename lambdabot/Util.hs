
module Util (
     join,
     split,
     breakOnGlue,
     snoc,
     after,
     split_first_word,
     debugStr,
     debugStrLn,
     lowerCaseString,
     Accessor (..),
     Serializer (..), stdSerializer, mapSerializer,
     readFM, writeFM, deleteFM,
     lookupSet, insertSet, deleteSet,
     getRandItem, stdGetRandItem,
     readM
) where

import Config
import Map                      (Map)
import qualified Map as M       (lookup, insert, delete, toList, fromList)

import Data.List                (intersperse, isPrefixOf)
import Data.Maybe               (catMaybes)
import Data.Set                 (elementOf, addToSet, delFromSet, Set)
import Data.Char                (isSpace, toLower)
import Control.Monad.State      (when,MonadIO(..))

import System.Random hiding (split)

------------------------------------------------------------------------

-- TODO: rename join, clashes with Monad.join

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
snoc :: a -- ^ Element to be added 
     -> [a] -- ^ List to add to
     -> [a] -- ^ Result: List ++ [Element]
snoc x xs = xs ++ [x]

-- | 'after' takes 2 strings, called the prefix and data. A necessary
--   precondition is that
--
--   > Data.List.isPrefixOf prefix data ===> True
--
--   'after' returns a string based on data, where the prefix has been
--   removed as well as any excess space characters. Example:
--
--   > after "This is" "This is a string" ===> "a string"
after :: String -- ^ Prefix string
      -> String -- ^ Data string
      -> String -- ^ Result: Data string with Prefix string and excess whitespace
	        --     removed
after [] ys     = dropWhile (==' ') ys
after (_:_) [] = error "after: (:) [] case"
after (x:xs) (y:ys)
  | x == y    = after xs ys
  | otherwise = error "after: /= case"

-- | Break a String into it's first word, and the rest of the string. Example:
--
-- > split_first_word "A fine day" ===> ("A", "fine day)
split_first_word :: String -- ^ String to be broken
		 -> (String, String)
split_first_word xs = (w, dropWhile isSpace xs')
  where (w, xs') = break isSpace xs

-- refactor, might be good for logging to file later
-- | 'debugStr' checks if we have the verbose flag turned on. If we have
--   it outputs the String given. Else, it is a no-op.

debugStr :: (MonadIO m) => String -> m ()
debugStr x = when (verbose config) $ liftIO (putStr x)

-- | 'debugStrLn' is a version of 'debugStr' that adds a newline to the end
--   of the string outputted.
debugStrLn :: (MonadIO m) => [Char] -> m ()
debugStrLn x = debugStr ( x ++ "\n" )

-- | 'lowerCaseString' transforms the string given to lower case.
--
-- > Example: lowerCaseString "MiXeDCaSe" ===> "mixedcase"
lowerCaseString :: String -> String
lowerCaseString = map toLower


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
getRandItem :: (RandomGen g) =>
	       [a] -- ^ The list to pick a random item from
	    -> g   -- ^ The RNG to use
	    -> (a, g) -- ^ A pair of the item, and the new RNG seed
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

-- | A Serializer provides a way for a type s to be written to and read from
--   a string. 
data Serializer s = Serializer {
  serialize   :: s -> String,
  deSerialize :: String -> Maybe s
}

stdSerializer :: (Show s, Read s) => Serializer s
stdSerializer = Serializer show readM
  
mapSerializer :: (Ord k, Show k, Show v, Read k, Read v) => Serializer (Map k v)
mapSerializer = Serializer {
  serialize = unlines . map show . M.toList,
  deSerialize = Just . M.fromList . catMaybes . map readM . lines
}


-- | like read, but catches failure in a monad.
readM :: (Monad m, Read a) => String -> m a
readM s = case [x | (x,t) <- reads s, ("","") <- lex t] of
        [x] -> return x
        []  -> fail "PreludeExts.readM: no parse"
        _   -> fail "PreludeExts.readM: ambiguous parse"
