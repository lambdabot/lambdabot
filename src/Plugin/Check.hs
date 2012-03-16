{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with QuickCheck
module Plugin.Check (theModule) where

import Plugin
import Plugin.Eval (eval)
import Lambdabot.Parser

plugin "Check"

instance Module CheckModule where
    moduleCmds = return
        [ (command "check")
            { help = do
                say "check <expr>"
                say "You have QuickCheck and 3 seconds. Prove something."
            , process = ios80 . check
            }
        ]

check :: String -> IO String
check src = 
    case parseExpr src of
        Left e  -> return e
        Right _ -> eval ("myquickcheck (" ++ src ++ ") `seq` hsep[]")
