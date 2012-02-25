{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with QuickCheck
module Plugin.Check where

import Plugin
import Plugin.Eval (plugs)
import Lambdabot.Parser

$(plugin "Check")

instance Module CheckModule () where
    moduleCmds   _     = ["check"]
    moduleHelp _ _     = "check <expr>\nYou have QuickCheck and 3 seconds. Prove something."
    process _ _ to _ s = ios80 to (check s)

check :: String -> IO String
check src = 
    case parseExpr src of
        Left e  -> return e
        Right _ -> plugs ("myquickcheck (" ++ src ++ ") `seq` hsep[]")
