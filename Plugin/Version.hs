{-# OPTIONS -cpp #-}

#include "config.h"

--
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Lambdabot version information
--
module Plugin.Version where

import Plugin

PLUGIN Version

instance Module VersionModule () where
    moduleCmds   _ = ["version", "source"]
    moduleHelp _ _ = "version/source. Report the build date, ghc version " ++ 
                     "and darcs repo of this bot"
    process_ _ _ _ = ios . return $ concat
                ["lambdabot 3p", PATCH_COUNT, ", ",
                 "GHC ", GHC_VERSION, " (", PLATFORM, " ", CPU, ")",
                 "\n", "darcs get ", REPO_PATH ]

