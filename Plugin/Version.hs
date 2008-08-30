{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Lambdabot version information
module Plugin.Version where

import Plugin
import Paths_lambdabot (version)
import Data.Version (showVersion)

$(plugin "Version")

instance Module VersionModule () where
    moduleCmds   _ = ["version"]
    moduleHelp _ _ = "version/source. Report the version " ++
                     "and darcs repo of this bot"
    process_ _ _ _ = ios . return $ concat
                [ "lambdabot ", showVersion version, "\n"
                , "darcs get http://code.haskell.org/lambdabot" ]
