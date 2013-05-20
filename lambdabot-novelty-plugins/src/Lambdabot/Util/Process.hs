-- Copyright (c) 2004-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

module Lambdabot.Util.Process
    ( run
    ) where

import System.Process

run :: FilePath -> String -> (String -> String) -> IO String
run binary src scrub = do
    (_,out,err) <- readProcessWithExitCode binary [] src
    let o = scrub out
        e = scrub err
    return $ case () of {_
        | null o && null e -> "Done."
        | null o           -> e
        | otherwise        -> o
    }
