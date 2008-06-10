{-# LANGUAGE CPP #-}

#include "config.h"

-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Lambdabot version information
module Plugin.Version where

import Plugin

PLUGIN Version

instance Module VersionModule () where
    moduleCmds   _ = ["version"]
    moduleHelp _ _ = "version/source. Report the build date, ghc version " ++
                     "and darcs repo of this bot"
    process_ _ _ _ = ios . return $ concat
                ["lambdabot 4p", PATCH_COUNT, ", ",
                 "GHC ", GHC_VERSION, " (", PLATFORM,
                 if null CPU then [] else " " ++ CPU , ")",
                 "\n", "darcs get ", REPO_PATH ]

