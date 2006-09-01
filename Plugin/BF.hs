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
    let o = scrub out
        e = scrub err
    return $ case () of {_
        | null o && null e -> "Done."
        | null o           -> e
        | otherwise        -> o
    }
    where 
    scrub = unlines . take 6 . map (' ':) . filter (not.null) 
          . map cleanit . lines

--
-- Clean up output
--
cleanit :: String -> String
cleanit s | Just _         <- terminated `matchRegex`    s = "Terminated\n"
          | Just _         <- halted     `matchRegex`    s = ""
          | otherwise      = s
    where terminated = mkRegex "waitForProc"
          halted     = mkRegex "Machine Halted"
