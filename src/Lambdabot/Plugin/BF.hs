-- Copyright (c) 2006 Jason Dagit - http://www.codersbase.com/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A plugin for the Haskell interpreter for the brainf*ck language
-- http://www.muppetlabs.com/~breadbox/bf/
module Lambdabot.Plugin.BF (theModule) where

import Lambdabot.Plugin
import Lambdabot.Util.Process
import Lambdabot.Util.Regex

import Data.Char

theModule = newModule
    { moduleCmds = return
        [ (command "bf")
            { help = say "bf <expr>. Evaluate a brainf*ck expression"
            , process = ios80 . bf
            }
        ]
    }

binary :: String
binary = "bf"

bf :: String -> IO String
bf src = run binary src scrub
  where scrub = unlines . take 6 . map (' ':) . filter (not.null) . map cleanit . lines

--
-- Clean up output
--
cleanit :: String -> String
cleanit s | terminated `matches'`    s = "Terminated\n"
          | otherwise                  = filter printable s
    where terminated = regex' "waitForProc"
          -- the printable ascii chars are in the range [32 .. 126]
          -- according to wikipedia:
          -- http://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters
          printable x = 31 < ord x && ord x < 127

