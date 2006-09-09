--
-- | Free theorems plugin, 
-- Don Stewart 2006
-- 
module Plugin.FT where

import Plugin
import Plugin.Type (query_ghci)

PLUGIN FT

instance Module FTModule () where
    moduleCmds _   = ["ft"]
    moduleHelp _ _ = "ft <ident>. Generate theorems for free"
    process_ _ _ s = (liftM unlines . lift . query_ghci ":t") s >>= ios . ft

binary :: String
binary = "./ft"

ft :: String -> IO String
ft src = do
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
          | otherwise      = filter isAscii s
    where terminated = mkRegex "waitForProc"
