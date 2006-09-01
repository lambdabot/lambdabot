--
-- Copyright (c) 2006 Jason Dagit - http://www.codersbase.com/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | A plugin for the Haskell interpreter for the brainf*ck language
--
-- http://www.muppetlabs.com/~breadbox/bf/
--
module Plugin.BF (theModule) where

import Plugin

PLUGIN BF

instance Module BFModule () where
    moduleCmds   _ = ["bf"]
    moduleHelp _ _ = "bf <expr>. Evaluate a bainf*ck expression"
    process _ _ to _ s = ios80 to (bf s)

binary :: String
binary = "./bf"

bf :: String -> IO String
bf src = do
    (out,err,_) <- popen binary [] (Just src)
    let o = unlines . take 6 . map (' ':) . lines . cleanit $ out
        e = unlines . take 6 . map (' ':) . lines . cleanit $ err
    return $ case () of {_
        | null o && null e -> "Done."
        | null o           -> e
        | otherwise        -> o
    }

--
-- Clean up output
--
cleanit :: String -> String
cleanit s | Just _         <- terminated `matchRegex`    s = "Terminated\n"
--        | Just _         <- hget       `matchRegex`    s = "Terminated\n"
          | otherwise      = s
    where terminated = mkRegex "waitForProc"

