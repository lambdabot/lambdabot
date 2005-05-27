--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Lambdabot version information
--
module Plugins.Version (theModule) where

import Lambdabot

#include "config.h"

newtype VersionModule = VersionModule ()

theModule :: MODULE
theModule = MODULE $ VersionModule ()

instance Module VersionModule () where
    moduleHelp _ _ = return $ "Report the build date, ghc version "
                           ++ "and darcs repo of this bot"
    moduleCmds   _ = return ["version", "source"]
    process      _ _ target _ _ =
        ircPrivmsg target $ concat
                ["lambdabot 3p", PATCH_COUNT, ", ",
                 "GHC ", GHC_VERSION, " (", PLATFORM, ")",
                 "\n", "darcs get ", REPO_PATH ]

