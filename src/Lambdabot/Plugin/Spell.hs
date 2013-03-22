{-# LANGUAGE PatternGuards #-}
-- Copyright (c) 2004-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | Interface to /aspell/, an open source spelling checker, from a
-- suggestion by Kai Engelhardt. Requires you to install aspell.
module Lambdabot.Plugin.Spell (theModule) where

import Lambdabot.Plugin

import Control.Monad.Trans
import Data.Char
import Data.List.Split
import Data.Maybe
import System.Process
import Text.Regex.TDFA


type Spell = ModuleT Bool LB

theModule :: Module Bool
theModule = newModule
    { moduleCmds = return
        [ (command "spell")
            { help = say helpStr
            , process = doSpell
            }
        , (command "spell-all")
            { help = say helpStr
            , process = spellAll
            }
        , (command "nazi-on")
            { privileged = True
            , help = say helpStr
            , process = const (nazi True)
            }
        , (command "nazi-off")
            { privileged = True
            , help = say helpStr
            , process = const (nazi False)
            }
        ]
    , moduleDefState = return False

    , contextual = \txt -> do
        alive <- readMS
        if alive then io (spellingNazi txt) >>= mapM_ say
                 else return ()
    }

helpStr :: String
helpStr = "spell <word>. Show spelling of word"

doSpell :: MonadIO m => [Char] -> Cmd m ()
doSpell [] = say "No word to spell."
doSpell s  = (say . showClean . take 5) =<< (io (spell s))

spellAll :: MonadIO m => [Char] -> Cmd m ()
spellAll [] = say "No phrase to spell."
spellAll s  = liftIO (spellingNazi s) >>= mapM_ say

nazi :: Bool -> Cmd (ModuleT Bool LB) ()
nazi True  = lift on  >> say "Spelling nazi engaged."
nazi False = lift off >> say "Spelling nazi disengaged."

on :: Spell ()
on  = writeMS True

off :: Spell ()
off = writeMS False

binary :: String
binary = "aspell"

args :: [String]
args = ["pipe"]

--
-- | Find the first misspelled word in the input line, and return plausible
-- output.
--
spellingNazi :: String -> IO [String]
spellingNazi lin = fmap (take 1 . concat) (mapM correct (words lin))
    where correct word = do
            var <- take 5 `fmap` spell word
            return $ if null var || any (equating' (map toLower) word) var
                then []
                else ["Did you mean " ++ listToStr "or" var ++ "?"]
          equating' f x y = f x == f y
--
-- | Return a list of possible spellings for a word
-- 'String' is a word to check the spelling of.
--
spell :: String -> IO [String]
spell word = spellWithArgs word []

spellWithArgs :: String -> [String] -> IO [String]
spellWithArgs word ex = do
    (_,out,err) <- readProcessWithExitCode binary (args++ex) word
    let o = fromMaybe [word] ((clean_ . lines) out)
        e = fromMaybe e      ((clean_ . lines) err)
    return $ case () of {_
        | null o && null e -> []
        | null o           -> e
        | otherwise        -> o
    }

--
-- Parse the output of aspell (would probably work for ispell too)
--
clean_ :: [String] -> Maybe [String]
clean_ (('@':'(':'#':')':_):rest) = clean' rest -- drop header
clean_ s = clean' s                             -- no header for some reason

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
clean' (('&':rest):_) = Just $ splitOn ", " (clean'' rest) -- suggestions
clean' _              = Just []                          -- not sure

clean'' :: String -> String
clean'' s = maybe s mrAfter (s =~~ pat)
    where pat  = "[^:]*: "    -- drop header
