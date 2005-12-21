--
-- Wed Dec 21 15:29:52 EST 2005
--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- A binding to Robert Dockins' Lambda Shell
--
--          darcs get http://www.eecs.tufts.edu/~rdocki01/lambda/
--

module Plugins.LShell where

import Lambdabot
import PosixCompat
import Control.Monad.Trans      (liftIO)
import Text.Regex
import Util                     (dropNL, (<$>))

newtype LShellModule = LShellModule ()

theModule :: MODULE
theModule = MODULE $ LShellModule ()

instance Module LShellModule () where
    moduleCmds   _ = ["lam"]
    moduleHelp _ _ = "Evaluate terms of the pure, untyped lambda calculus" <$>
                     "darcs get http://www.eecs.tufts.edu/~rdocki01/lambda"

    -- rule out attempts to do IO
    process_ _ _ s | Just _ <- cmd  `matchRegex` s = end
      where end  = return ["Invalid command"]
            cmd  = mkRegex "^ *:"

    process_ _ _ s = liftIO $ lambdashell s 

------------------------------------------------------------------------

lambdashell :: String -> IO [String]
lambdashell src = do
    (out,_,_) <- popen "./lambda" [] $ Just $ 
                    ":load State/prelude.lam" <$> src <$> ":quit"
    let o = dropNL . doclean . last . init . drop 11 . lines $ out
    return [o]

--
-- Clean up djinn output
--
doclean :: String -> String
doclean s | Just (a,_,b,_) <- prompt `matchRegexAll` s = a ++ doclean b
          | otherwise      = s
    where
        prompt = mkRegex ">[^\n]*\n"
