--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Interface to /aspell/, an open source spelling checker.
-- Requires you to install aspell.
--
module Spell (spell) where

import PosixCompat
import Util         (split)
import Data.Maybe   (fromMaybe)
import Text.Regex

binary :: String
binary = "aspell"

args :: [String]
args = ["pipe"]

--
-- | Return a list of possible spellings for a word
-- 'String' is a word to check the spelling of.
--
spell :: String -> IO [String]
spell word = spellWithDict word Nothing [] 

--
-- | Like 'spell', but you can specify which dictionary and pass extra
-- arguments to aspell.
--
spellWithDict :: String -> Maybe Dictionary -> [String] -> IO [String]
spellWithDict word (Just d) ex = spellWithDict word Nothing ("--master":show d:ex)
spellWithDict word Nothing  ex = do
    (out,err,_) <- popen binary (args++ex) (Just word)
    let o = fromMaybe [word] (clean $ lines out)
        e = fromMaybe e      (clean $ lines err)
    return $ case () of {_
        | null o && null e -> []
        | null o           -> e
        | otherwise        -> o
    }

--
-- Parse the output of aspell (would probably work for ispell too)
--
clean :: [String] -> Maybe [String]
clean (('@':'(':'#':')':_):rest) = clean' rest -- drop header
clean s = clean' s                             -- no header for some reason

--
-- Parse rest of aspell output.
--
-- Grammar is:
--      OK          ::=  *
--      Suggestions ::= & <original> <count> <offset>: <miss>, <miss>, ...
--      None        ::= # <original> <offset>
--
clean' :: [String] -> Maybe [String]
clean' (('*':_):_)    = Nothing                          -- correct spelling
clean' (('#':_):_)    = Just []                          -- no match
clean' (('&':rest):_) = Just $ split ", " (clean'' rest) -- suggestions
clean' _              = Just []                          -- not sure

clean'' :: String -> String
clean'' s
    | Just (_,_,m,_) <- pat `matchRegexAll` s = m
    | otherwise = s
    where
        pat  = mkRegex "[^:]*: "    -- drop header

--
-- Alternate dictionaries, currently just lambdabot's. The idea was to
-- have a dictionary for lambdabot commands, and aspell rather than
-- levinshtein for spelling errors. Turns out to be quite complicated,
-- for negligible gain.
--
data Dictionary
    = Lambdabot -- ^ Dictionary of lambdabot commands (it's its own language)

-- path to the master dictionary
instance Show Dictionary where
    showsPrec _ Lambdabot = showString "data/lambdabot"

