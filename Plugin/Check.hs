{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards #-}
-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with QuickCheck
module Plugin.Check where

import File (findFile)
import Plugin
import Plugin.Eval (plugs)
import Lambdabot.Parser

import qualified Text.Regex as R
import Codec.Binary.UTF8.String (decodeString)

$(plugin "Check")

instance Module CheckModule () where
    moduleCmds   _     = ["check"]
    moduleHelp _ _     = "check <expr>\nYou have QuickCheck and 3 seconds. Prove something."
    process _ _ to _ s = ios80 to (check s)

binary :: String
binary = "mueval"

check :: String -> IO String
check src = plugs ("myquickcheck $ " ++ src)
