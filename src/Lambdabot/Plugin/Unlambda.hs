-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | A plugin for the Haskell interpreter for the unlambda language
--
-- http://www.madore.org/~david/programs/unlambda/
module Lambdabot.Plugin.Unlambda (theModule) where

import Lambdabot.Plugin

theModule = newModule
    { moduleCmds = return
        [ (command "unlambda")
            { help = say "unlambda <expr>. Evaluate an unlambda expression"
            , process = ios80 . unlambda
            }
        ]
    }

binary :: String
binary = "unlambda"

unlambda :: String -> IO String
unlambda src = run binary src scrub
  where scrub = unlines . take 6 . map (' ':) . lines . cleanit

cleanit :: String -> String
cleanit s | terminated `matches'` s = "Terminated\n"
          | otherwise               = s
    where terminated = regex' "waitForProc"
