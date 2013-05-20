{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Haskell
    ( evalPrefixes
    , trustedPackages
    
    , djinnBinary
    , ghcBinary
    , ghciBinary
    , hoogleBinary
    , muevalBinary
    ) where

import Lambdabot.Config

config "evalPrefixes"       [t| [String]                |] [| [">"]         |]

trustedPkgs :: [String]
trustedPkgs =
    [ "array"
    , "base"
    , "bytestring"
    , "containers"
    , "lambdabot"
    , "random"
    ]

configWithMerge [| (++) |] "trustedPackages"    [t| [String] |] [| trustedPkgs   |]

config "djinnBinary"        [t| String                  |] [| "djinn"       |]
config "ghcBinary"          [t| String                  |] [| "ghc"         |]
config "ghciBinary"         [t| String                  |] [| "ghci"        |]
config "hoogleBinary"       [t| String                  |] [| "hoogle"      |]
config "muevalBinary"       [t| String                  |] [| "mueval"      |]

