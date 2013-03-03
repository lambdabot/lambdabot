-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | String and other utilities
module Lambdabot.Util (
        split,
        breakOnGlue,
        clean,
        dropSpace,
        dropSpaceEnd,
        dropNL,
        splitFirstWord,
        firstWord,
        limitStr,
        listToStr, showWidth,
        showClean,
        expandTab,
        closest, closests,
        withMWriter, parIO, timeout,
        arePrefixesWithSpaceOf, arePrefixesOf,

        pprKeys,

        isLeft, isRight, unEither,

        io,

        random, insult, confirmation
    ) where

import Data.List                (intercalate, isPrefixOf)
import Data.Char                (isSpace)
import Data.Random
import Control.Monad.State      (MonadIO(..))

import qualified Data.Map as M
import Data.IORef               (newIORef, readIORef, writeIORef)

import Control.Concurrent       (MVar, newEmptyMVar, takeMVar, tryPutMVar, putMVar,
                                 forkIO, killThread, threadDelay)
import Control.Exception        (bracket)

------------------------------------------------------------------------

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

-- | Truncate a string to the specified length, putting ellipses at the
-- end if necessary.
limitStr :: Int -> String -> String
limitStr n s = let (b, t) = splitAt n s in
           if null t then b else take (n-3) b ++ "..."

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

-- | Pick a random element of the list.
random :: MonadIO m => [a] -> m a
random = io . sample . randomElement

------------------------------------------------------------------------

-- | 'dropSpace' takes as input a String and strips spaces from the
--   prefix as well as the suffix of the String. Example:
--
-- > dropSpace "   abc  " ===> "abc"
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f

-- | Drop space from the end of the string
dropSpaceEnd :: [Char] -> [Char]
dropSpaceEnd = reverse . dropWhile isSpace . reverse

-- | 'clean' takes a Char x and returns [x] unless the Char is '\CR' in
--   case [] is returned.
clean :: Char -> [Char]
clean x | x == '\CR' = []
        | otherwise  = [x]

------------------------------------------------------------------------

-- | show a list without heavyweight formatting
showClean :: (Show a) => [a] -> String
showClean = intercalate " " . map (init . tail . show)

dropNL :: [Char] -> [Char]
dropNL = reverse . dropWhile (== '\n') . reverse

-- | untab an string
expandTab :: Int -> String -> String
expandTab w = go 0
  where
    go _ []         = []
    go i ('\t':xs)  = replicate (w - i `mod` w) ' ' ++ go 0 xs
    go i (x:xs)     = x : go (i+1) xs

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
  m  <- newEmptyMVar
  c1 <- forkIO $ putMVar m =<< a1
  c2 <- forkIO $ putMVar m =<< a2
  r  <- takeMVar m
  -- killThread blocks until the thread has been killed.  Therefore, we call
  -- killThread asynchronously in case one thread is blocked in a foreign
  -- call.
  _  <- forkIO $ killThread c1 >> killThread c2
  return r

-- | run an action with a timeout
timeout :: Int -> IO a -> IO (Maybe a)
timeout n a = parIO (Just `fmap` a) (threadDelay n >> return Nothing)

------------------------------------------------------------------------

--  | Print map keys
pprKeys :: (Show k) => M.Map k a -> String
pprKeys = showClean . M.keys

-- | Two functions that really should be in Data.Either
isLeft, isRight :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
isRight         = not . isLeft

-- | Another useful Either function to easily get out of an Either
unEither :: Either a a -> a
unEither = either id id


-- convenience:
io :: MonadIO m => IO a -> m a
io = liftIO
{-# INLINE io #-}


arePrefixesWithSpaceOf :: [String] -> String -> Bool
arePrefixesWithSpaceOf = arePrefixesOf . map (++ " ")

arePrefixesOf :: [String] -> String -> Bool
arePrefixesOf = flip (any . flip isPrefixOf)

-- | Show a number, padded to the left with zeroes up to the specified width
showWidth :: Int    -- ^ Width to fill to
          -> Int    -- ^ Number to show
          -> String -- ^ Padded string
showWidth width n = zeroes ++ num
    where num    = show n
          zeroes = replicate (width - length num) '0'

--
-- Amusing insults from OpenBSD sudo
--
insult :: [String]
insult =
   ["Just what do you think you're doing Dave?",
    "It can only be attributed to human error.",
    "That's something I cannot allow to happen.",
    "My mind is going. I can feel it.",
    "Sorry about this, I know it's a bit silly.",
    "Take a stress pill and think things over.",
    "This mission is too important for me to allow you to jeopardize it.",
    "I feel much better now.",

    "Wrong!  You cheating scum!",
    "And you call yourself a Rocket Scientist!",
    "And you call yourself a Rocket Surgeon!",
    "Where did you learn to type?",
    "Are you on drugs?",
    "My pet ferret can type better than you!",
    "You type like i drive.",
    "Do you think like you type?",
    "Your mind just hasn't been the same since the electro-shock, has it?",
    "I don't think I can be your friend on Facebook anymore.",

    "Maybe if you used more than just two fingers...",
    "BOB says:  You seem to have forgotten your passwd, enter another!",
    "stty: unknown mode: doofus",
    "I can't hear you -- I'm using the scrambler.",
    "The more you drive -- the dumber you get.",
    "Listen, broccoli brains, I don't have time to listen to this trash.",
    "I've seen penguins that can type better than that.",
    "Have you considered trying to match wits with a rutabaga?",
    "You speak an infinite deal of nothing.",

    -- other
    "Are you typing with your feet?",
    "Abort, Retry, Panic?",

    -- More haskellish insults
    "You untyped fool!",
    "My brain just exploded",

    -- some more friendly replies
    "I am sorry.","Sorry.",
    "Maybe you made a typo?",
    "Just try something else.",
    "There are some things that I just don't know.",
    "Whoa.",
    -- sometimes don't insult at all
    ":(",":(",
    "","",""
    ]

--
-- Some more interesting confirmations for @remember and @where
--
confirmation :: [String]
confirmation =
   ["Done.","Done.",
    "Okay.",
    "I will remember.",
    "Good to know.",
    "It is stored.",
    "I will never forget.",
    "It is forever etched in my memory.",
    "Nice!"
   ]
