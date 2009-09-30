{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- Copyright (c) 2006 Jason Dagit - http://www.codersbase.com/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A plugin for the Haskell interpreter for the brainf*ck language
-- http://www.muppetlabs.com/~breadbox/bf/
module Plugin.BF (theModule) where

import Plugin

$(plugin "BF")

instance Module BFModule () where
    moduleCmds   _     = ["bf"]
    moduleHelp _ _     = "bf <expr>. Evaluate a brainf*ck expression"
    process _ _ to _ s = ios80 to (bf s)

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

