--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | String and other utilities
--
module Lib.Util (
        concatWith,
        split, split2,
        breakOnGlue,
        clean,
        dropSpace,
        dropNL,
        snoc,
        after,
        splitFirstWord,
        firstWord,
        debugStr,
        debugStrLn,
        lowerCaseString, upperCaseString,
        upperize, lowerize,
        quote,
        listToStr,
        getRandItem, stdGetRandItem, randomElem,
        showClean,
        expandTab,
        closest, closests,
        withMWriter, parIO, timeout,
        choice,

        (</>), (<.>), (<+>), (<>), (<$>),
        basename, dirname, dropSuffix, joinPath,

        addList, mapMaybeMap, insertUpd, 
    ) where

import Data.List                (intersperse, isPrefixOf)
import Data.Char                (isSpace, toLower, toUpper)
import Data.Maybe
import Control.Monad.State      (MonadIO(..))

import qualified Data.Map as M
import Data.IORef               (newIORef, readIORef, writeIORef)

import Control.Concurrent       (MVar, newEmptyMVar, takeMVar, tryPutMVar, putMVar,
                                 forkIO, killThread, threadDelay)
import Control.Exception        (bracket)

import System.Random hiding (split)

------------------------------------------------------------------------

-- | 'concatWith' joins lists with the given glue elements. Example:
--
-- > concatWith ", " ["one","two","three"] ===> "one, two, three"
concatWith  :: [a]   -- ^ Glue to join with
            -> [[a]] -- ^ Elements to glue together
            -> [a]   -- ^ Result: glued-together list

concatWith glue xs = (concat . intersperse glue) xs


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

-- a variant?
split2 :: Char -> Int -> String -> [String]
split2 c i s =
        let fn 0 t = t:[]
            fn j t = let (xs,ys) = break (== c) t
                     in case ys of
                        [] -> xs:[]
                        _  -> xs: fn (j-1) (tail ys)
        in fn (i-1) s


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
after [] ys     = dropWhile isSpace ys
after (_:_) [] = error "after: (:) [] case"
after (x:xs) (y:ys)
  | x == y    = after xs ys
  | otherwise = error "after: /= case"

-- | Break a String into it's first word, and the rest of the string. Example:
--
-- > split_first_word "A fine day" ===> ("A", "fine day)
splitFirstWord :: String -- ^ String to be broken
                 -> (String, String)
splitFirstWord xs = (w, dropWhile isSpace xs')
  where (w, xs') = break isSpace xs

-- | Get the first word of a string. Example:
--
-- > first_word "This is a fine day" ===> "This"
firstWord :: String -> String
firstWord = takeWhile (not . isSpace)

-- refactor, might be good for logging to file later
-- | 'debugStr' checks if we have the verbose flag turned on. If we have
--   it outputs the String given. Else, it is a no-op.

debugStr :: (MonadIO m) => String -> m ()
debugStr = liftIO . putStr

-- | 'debugStrLn' is a version of 'debugStr' that adds a newline to the end
--   of the string outputted.
debugStrLn :: (MonadIO m) => [Char] -> m ()
debugStrLn x = debugStr (x ++ "\n")

-- | 'lowerCaseString' transforms the string given to lower case.
--
-- > Example: lowerCaseString "MiXeDCaSe" ===> "mixedcase"
lowerCaseString :: String -> String
lowerCaseString = map toLower

-- | 'upperCaseString' transforms the string given to upper case.
--
-- > Example: upperCaseString "MiXeDcaSe" ===> "MIXEDCASE"
upperCaseString :: String -> String
upperCaseString = map toUpper

-- | 'lowerize' forces the first char of a string to be lowercase.
--   if the string is empty, the empty string is returned.
lowerize :: String -> String
lowerize [] = []
lowerize (c:cs) = toLower c:cs

-- | 'upperize' forces the first char of a string to be uppercase.
--   if the string is empty, the empty string is returned.
upperize :: String -> String
upperize [] = []
upperize (c:cs) = toUpper c:cs

-- | 'quote' puts a string into quotes but does not escape quotes in
--   the string itself.
quote  :: String -> String
quote x = "\"" ++ x ++ "\""

-- | Form a list of terms using a single conjunction. Example:
--
-- > listToStr "and" ["a", "b", "c"] ===> "a, b and c"
listToStr :: String -> [String] -> String
listToStr _    []           = []
listToStr conj (item:items) =
  let listToStr' [] = []
      listToStr' [y] = concat [" ", conj, " ", y]
      listToStr' (y:ys) = concat [", ", y, listToStr' ys]
  in  item ++ listToStr' items

------------------------------------------------------------------------

-- | 'getRandItem' takes as input a list and a random number generator. It
--   then returns a random element from the list, paired with the altered
--   state of the RNG
getRandItem :: (RandomGen g) =>
               [a] -- ^ The list to pick a random item from
            -> g   -- ^ The RNG to use
            -> (a, g) -- ^ A pair of the item, and the new RNG seed
getRandItem [] _       = error "getRandItem: empty list"
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
stdGetRandItem = getStdRandom . getRandItem

randomElem :: [a] -> IO a
randomElem = stdGetRandItem

------------------------------------------------------------------------

-- | 'dropSpace' takes as input a String and strips spaces from the
--   prefix as well as the postfix of the String. Example:
--
-- > dropSpace "   abc  " ===> "abc"
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f

-- | 'clean' takes a Char x and returns [x] unless the Char is '\CR' in
--   case [] is returned.
clean :: Char -> [Char]
clean x | x == '\CR' = []
        | otherwise  = [x]

------------------------------------------------------------------------

-- | show a list without heavyweight formatting
showClean :: (Show a) => [a] -> String
showClean = concatWith " " . map (init . tail . show)

dropNL :: [Char] -> [Char]
dropNL = reverse . dropWhile (== '\n') . reverse

-- | untab an string
expandTab :: String -> String
expandTab []        = []
expandTab ('\t':xs) = ' ':' ':' ':' ':' ':' ':' ':' ':expandTab xs
expandTab (x:xs)    = x : expandTab xs

------------------------------------------------------------------------

--
-- | Find string in list with smallest levenshtein distance from first
-- argument, return the string and the distance from pat it is.  Will
-- return the alphabetically first match if there are multiple matches
-- (this may not be desirable, e.g. "mroe" -> "moo", not "more"
--
closest :: String -> [String] -> (Int,String)
closest pat ss = minimum ls
    where
        ls = map (\s -> (levenshtein pat s,s)) ss

closests :: String -> [String] -> (Int,[String])
closests pat ss =
    let (m,_) = minimum ls
    in (m, map snd (filter ((m ==) . fst) ls))
    where
        ls = map (\s -> (levenshtein pat s,s)) ss

--
-- | Levenshtein edit-distance algorithm
-- Translated from an Erlang version by Fredrik Svensson and Adam Lindberg
--
levenshtein :: String -> String -> Int
levenshtein [] [] = 0
levenshtein s  [] = length s
levenshtein [] s  = length s
levenshtein s  t  = lvn s t [0..length t] 1

lvn :: String -> String -> [Int] -> Int -> Int
lvn [] _ dl _ = last dl
lvn (s:ss) t dl n = lvn ss t (lvn' t dl s [n] n) (n + 1)

lvn' :: String -> [Int] -> Char -> [Int] -> Int -> [Int]
lvn' [] _ _ ndl _ = ndl
lvn' (t:ts) (dlh:dlt) c ndl ld | length dlt > 0 = lvn' ts dlt c (ndl ++ [m]) m
    where
        m = foldl1 min [ld + 1, head dlt + 1, dlh + (dif t c)]
lvn' _ _ _ _  _  = error "levenshtein, ran out of numbers"

dif :: Char -> Char -> Int
dif = (fromEnum .) . (/=)

{-
--
-- naive implementation, O(2^n)
-- Too slow after around d = 8
--
-- V. I. Levenshtein. Binary codes capable of correcting deletions,
-- insertions and reversals. Doklady Akademii Nauk SSSR 163(4) p845-848,
-- 1965
--
-- A Guided Tour to Approximate String Matching, G. Navarro
--
levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein [] [] = 0
levenshtein s  [] = length s
levenshtein [] s  = length s
levenshtein (s:ss) (t:ts)   =
    min3 (eq + (levenshtein  ss ts))
         (1  + (levenshtein (ss++[s]) ts))
         (1  + (levenshtein  ss (ts++[t])))
        where
          eq         = fromEnum (s /= t)
          min3 a b c = min c (min a b)
-}

------------------------------------------------------------------------

-- | Thread-safe modification of an MVar.
withMWriter :: MVar a -> (a -> (a -> IO ()) -> IO b) -> IO b
withMWriter mvar f = bracket
  (do x <- takeMVar mvar; ref <- newIORef x; return (x,ref))
  (\(_,ref) -> tryPutMVar mvar =<< readIORef ref)
  (\(x,ref) -> f x $ writeIORef ref)


-- stolen from
-- http://www.haskell.org/pipermail/haskell-cafe/2005-January/008314.html
parIO :: IO a -> IO a -> IO a
parIO a1 a2 = do
  m <- newEmptyMVar
  c1 <- forkIO $ putMVar m =<< a1
  c2 <- forkIO $ putMVar m =<< a2
  r <- takeMVar m
  killThread c1
  killThread c2
  return r

timeout :: Int -> IO a -> IO (Maybe a)
timeout n a = parIO (Just `fmap` a) (threadDelay n >> return Nothing)

------------------------------------------------------------------------

-- some filename manipulation stuff

--
-- | </>, <.> : join two path components
--
infixr 6 </>
infixr 6 <.>
infixr 6 <+>
infixr 6 <>
infixr 6 <$>

(</>), (<.>), (<+>), (<>), (<$>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <+> b = b
a  <+> b = a ++ " " ++ b

[] <> b = b
a  <> b = a ++ b

[] <$> b = b
a  <$> b = a ++ "\n" ++ b

basename :: FilePath -> FilePath
basename = reverse . takeWhile ('/' /=) . reverse

dirname :: FilePath -> FilePath
dirname p  =
    case reverse $ dropWhile (/= '/') $ reverse p of
        [] -> "."
        p' -> p'

dropSuffix :: FilePath -> FilePath
dropSuffix = reverse . tail . dropWhile ('.' /=) . reverse

joinPath :: FilePath -> FilePath -> FilePath
joinPath p q =
    case reverse p of
      '/':_ -> p ++ q
      []    -> q
      _     -> p ++ "/" ++ q

{-# INLINE choice #-}
choice :: (r -> Bool) -> (r -> a) -> (r -> a) -> (r -> a)
choice p f g x = if p x then f x else g x

-- Generalizations:
-- choice :: ArrowChoice (~>) => r ~> Bool -> r ~> a -> r ~> a -> r ~> a
-- choice :: Monad m => m Bool -> m a -> m a -> m a

------------------------------------------------------------------------

addList :: (Ord k) => [(k,a)] -> M.Map k a -> M.Map k a
addList l m = M.union (M.fromList l) m
{-# INLINE addList #-}

-- | Data.Maybe.mapMaybe for Maps
mapMaybeMap :: Ord k => (a -> Maybe b) -> M.Map k a -> M.Map k b
mapMaybeMap f = fmap fromJust . M.filter isJust . fmap f

-- | This makes way more sense than @insertWith@ because we don't need to
--   remember the order of arguments of @f@.
insertUpd :: Ord k => (a -> a) -> k -> a -> M.Map k a -> M.Map k a
insertUpd f = M.insertWith (\_ -> f)
