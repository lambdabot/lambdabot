-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

{-# LANGUAGE FlexibleContexts #-}

-- | String and other utilities
module Lambdabot.Util (
        strip,
        dropFromEnd,
        splitFirstWord,
        limitStr,
        listToStr,
        showClean,
        expandTab,
        arePrefixesWithSpaceOf,
        arePrefixesOf,

        io,
        forkUnmasked,

        random,
        randomFailureMsg,
        randomSuccessMsg
    ) where

import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Random
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control
import Lambdabot.Config
import Lambdabot.Config.Core

------------------------------------------------------------------------

-- | Break a String into it's first word, and the rest of the string. Example:
--
-- > split_first_word "A fine day" ===> ("A", "fine day)
splitFirstWord :: String -- ^ String to be broken
                 -> (String, String)
splitFirstWord xs = (w, dropWhile isSpace xs')
  where (w, xs') = break isSpace xs

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

-- | 'strip' takes as input a predicate and a list and strips
--   elements matching the predicate from the prefix as well as
--   the suffix of the list. Example:
--
-- > strip isSpace "   abc  " ===> "abc"
strip :: (a -> Bool) -> [a] -> [a]
strip p = let f = reverse . dropWhile p in f . f

-- | Drop elements matching a predicate from the end of a list
dropFromEnd :: (a -> Bool) -> [a] -> [a]
dropFromEnd p = reverse . dropWhile p . reverse

------------------------------------------------------------------------

-- | show a list without heavyweight formatting
-- NB: assumes show instance outputs a quoted 'String'.
-- under that assumption, strips the outer quotes.
showClean :: (Show a) => [a] -> String
showClean = intercalate " " . map (init . tail . show)

-- | untab an string
expandTab :: Int -> String -> String
expandTab w = go 0
  where
    go _ []         = []
    go i ('\t':xs)  = replicate (w - i `mod` w) ' ' ++ go 0 xs
    go i (x:xs)     = x : go (i+1) xs

------------------------------------------------------------------------

-- convenience:
io :: MonadIO m => IO a -> m a
io = liftIO
{-# INLINE io #-}

forkUnmasked :: MonadBaseControl IO m => m () -> m ThreadId
forkUnmasked m = forkWithUnmask $ \unmask -> unmask m

arePrefixesWithSpaceOf :: [String] -> String -> Bool
arePrefixesWithSpaceOf = arePrefixesOf . map (++ " ")

arePrefixesOf :: [String] -> String -> Bool
arePrefixesOf = flip (any . flip isPrefixOf)

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
    "My brain just exploded"
    ]

--
-- some more friendly replies
--
apology :: [String]
apology =
   ["I am sorry.","Sorry.",
    "Maybe you made a typo?",
    "Just try something else.",
    "There are some things that I just don't know.",
    "Whoa.",
    ":(",":(",
    "","",""
    ]

randomFailureMsg :: (MonadIO m, MonadConfig m) => m String
randomFailureMsg = do
    useInsults <- getConfig enableInsults
    random (if useInsults then insult ++ apology else apology)

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

randomSuccessMsg :: MonadIO m => m String
randomSuccessMsg = random confirmation
