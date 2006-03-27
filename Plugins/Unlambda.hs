--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | A plugin for the Haskell interpreter for the unlambda language
--
-- http://www.madore.org/~david/programs/unlambda/
--

module Plugins.Unlambda (theModule) where

import Lambdabot hiding (clean)
import Text.Regex
import PosixCompat

newtype UnlambdaModule = UnlambdaModule ()

theModule :: MODULE
theModule = MODULE $ UnlambdaModule ()

instance Module UnlambdaModule () where
    moduleCmds   _ = ["unlambda"]
    moduleHelp _ _ = "unlambda <expr>. Evaluate an unlambda expression"
    process_ _ _ s = liftIO (unlambda s) >>= return . (:[])

------------------------------------------------------------------------

binary :: String
binary = "./unlambda"

unlambda :: String -> IO String
unlambda src = do
    (out,err,_) <- popen binary [] (Just src)
    let o = unlines . take 20 . lines . clean $ out
        e = unlines . take 20 . lines . clean $ err
    return $ case () of {_
        | null o && null e -> "Done."
        | null o           -> e
        | otherwise        -> o
    }

--
-- Clean up output
--
clean :: String -> String
clean s | Just _         <- terminated `matchRegex`    s = "Terminated\n"
--      | Just _         <- hget       `matchRegex`    s = "Terminated\n"
        | otherwise      = s
    where terminated = mkRegex "waitForProc"

