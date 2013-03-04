{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Core
    ( commandPrefixes
    , disabledCommands
    , evalPrefixes
    , ghci
    , onStartupCmds
    , outputDir
    , proxy
    , verbose
    ) where

import Lambdabot.Config

-------------------------------------
-- Core configuration variables

config "commandPrefixes"    [t| [String]                |] [| ["@", "?"]    |]
config "disabledCommands"   [t| [String]                |] [| []            |]
config "evalPrefixes"       [t| [String]                |] [| [">"]         |]
config "ghci"               [t| String                  |] [| "ghci"        |]
config "onStartupCmds"      [t| [String]                |] [| []            |]
config "outputDir"          [t| FilePath                |] [| "State/"      |]
config "proxy"              [t| Maybe ([Char], Integer) |] [| Nothing       |]
config "verbose"            [t| Bool                    |] [| False         |]


