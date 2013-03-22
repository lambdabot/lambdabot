{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Core
    ( commandPrefixes
    , disabledCommands
    , evalPrefixes
    , onStartupCmds
    , outputDir
    , proxy
    , uncaughtExceptionHandler
    , verbose
    
    , aspellBinary
    , bfBinary
    , djinnBinary
    , ghcBinary
    , ghciBinary
    , hoogleBinary
    , muevalBinary
    , unlambdaBinary
    ) where

import Lambdabot.Config
import Lambdabot.Error
import {-# SOURCE #-} Lambdabot.Monad

import Control.Monad.Trans
import Network.HTTP.Proxy

-------------------------------------
-- Core configuration variables

config "commandPrefixes"    [t| [String]                |] [| ["@", "?"]    |]
config "disabledCommands"   [t| [String]                |] [| []            |]
config "evalPrefixes"       [t| [String]                |] [| [">"]         |]
config "onStartupCmds"      [t| [String]                |] [| []            |]
config "outputDir"          [t| FilePath                |] [| "State/"      |]
config "proxy"              [t| Proxy                   |] [| NoProxy       |]
config "verbose"            [t| Bool                    |] [| False         |]

-------------------------------------
-- Program names/locations

config "aspellBinary"       [t| String                  |] [| "aspell"      |]
config "bfBinary"           [t| String                  |] [| "bf"          |]
config "djinnBinary"        [t| String                  |] [| "djinn"       |]
config "ghcBinary"          [t| String                  |] [| "ghc"         |]
config "ghciBinary"         [t| String                  |] [| "ghci"        |]
config "hoogleBinary"       [t| String                  |] [| "hoogle"      |]
config "muevalBinary"       [t| String                  |] [| "mueval"      |]
config "unlambdaBinary"     [t| String                  |] [| "unlambda"    |]

-------------------------------------
-- Top level exception-handler

defaultIrcHandler :: IRCError -> LB ()
defaultIrcHandler = liftIO . putStrLn . ("Main: caught (and ignoring) "++) . show

config "uncaughtExceptionHandler" [t| IRCError -> LB () |] [| defaultIrcHandler |]
