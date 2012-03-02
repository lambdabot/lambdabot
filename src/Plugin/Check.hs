{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with QuickCheck
module Plugin.Check (theModule) where

import Plugin
import Plugin.Eval (plugs)
import Lambdabot.Parser

plugin "Check"

instance Module CheckModule where
    moduleCmds _ =
        [ (command "check")
            { help = do
                say "check <expr>"
                say "You have QuickCheck and 3 seconds. Prove something."
            , process = \s -> do
                to <- getTarget
                ios80 to (check s) >>= mapM_ say
            }
        ]

check :: String -> IO String
check src = 
    case parseExpr src of
        Left e  -> return e
        Right _ -> plugs ("myquickcheck (" ++ src ++ ") `seq` hsep[]")
