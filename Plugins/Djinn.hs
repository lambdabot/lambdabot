--
-- Copyright (c) 2005 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
-- 
-- Written: Mon Dec 12 10:16:56 EST 2005
--

--
-- | A binding to Djinn.
--
module Plugins.Djinn (theModule) where

import Lambdabot      hiding  (clean)
import PosixCompat

import Control.Monad.Trans      ( liftIO )
import Text.Regex

newtype DjinnModule = DjinnModule ()

theModule :: MODULE
theModule = MODULE $ DjinnModule ()

instance Module DjinnModule () where
        moduleHelp _ _ = return $ "@djinn <type>\nGenerates Haskell code from a type."
        moduleCmds   _ = return ["djinn"]
        process _ _ src "djinn" s = do o <- liftIO $ djinn s
                                       ircPrivmsg src o
        process _ _ _ _ _ = error "DjinnModule: invalid command"

binary :: String
binary = "./djinn"

djinn :: String -> IO String
djinn src = do
    (out,err,_) <- popen binary [] (Just ("x ? " ++ src ++ "\n:q"))
    let o = dropNL . clean . unlines . init . drop 2 . lines $ out
        e = clean $ err
    return $ case () of {_
        | null o && null e -> "Terminated\n"
        | null o           -> e
        | otherwise        -> o
    }
    where
        dropNL = reverse . dropWhile (== '\n') . reverse

--
-- Clean up djinn output
--
clean :: String -> String
clean s | Just (a,_,b,_) <- prompt `matchRegexAll` s = a ++ clean b
        | otherwise      = s
    where
        prompt = mkRegex "Djinn>[^\n]*\n"

