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
    commands     _ = return ["version"]
    process      _ _ target _ _ =
        ircPrivmsg target $ "lambdabot :: " ++ 
                            "ghc-" ++ GHC_VERSION ++
                            " (" ++ BUILD_DATE ++ ")" ++
                            " (" ++ PLATFORM ++ ")" ++ "\n" ++
                            "darcs get http://www.cse.unsw.edu.au/~dons/lambdabot"

