{-# OPTIONS -cpp #-}

-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons

module VersionModule where

import IRC

#include "config.h"

newtype VersionModule = VersionModule ()

theModule :: MODULE
theModule = MODULE versionModule

versionModule :: VersionModule
versionModule = VersionModule ()

instance Module VersionModule where
    moduleName   _ = return "version"
    moduleSticky _ = False
    moduleHelp _ _ = return "Report the build date, ghc version and darcs repo of this bot"
    commands     _ = return ["version"]
    process      _ _ target _ _ =
        ircPrivmsg target $ 
            BUILD_DATE ++ ", " ++
            "GHC " ++ GHC_VERSION ++
            " (" ++ PLATFORM ++ ")" ++ "\n" ++
            "darcs get " ++ REPO_PATH

