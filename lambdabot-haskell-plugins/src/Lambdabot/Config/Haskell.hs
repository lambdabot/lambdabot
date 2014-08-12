{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Haskell
    ( evalPrefixes
    , languageExts
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
    , "lambdabot-haskell-plugins"
    , "lambdabot-trusted"
    , "random"
    ]

configWithMerge [| (++) |] "trustedPackages"    [t| [String] |] [| trustedPkgs   |]

-- extensions to enable for the interpreted expression
-- (and probably also L.hs if it doesn't already have these set)
defaultExts :: [String]
defaultExts =
    [ "ImplicitPrelude" -- workaround for bug in hint package
    , "ExtendedDefaultRules"
    ]

configWithMerge [| (++) |] "languageExts"   [t| [String] |] [| defaultExts |]


config "djinnBinary"        [t| String                  |] [| "djinn"       |]
config "ghcBinary"          [t| String                  |] [| "ghc"         |]
config "ghciBinary"         [t| String                  |] [| "ghci"        |]
config "hoogleBinary"       [t| String                  |] [| "hoogle"      |]
config "muevalBinary"       [t| String                  |] [| "mueval"      |]

