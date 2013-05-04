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
    
    , replaceRootLogger
    , consoleLogHandle
    , consoleLogLevel
    , consoleLogFormat
    
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
import Lambdabot.Logging

import Control.Exception
import Network.HTTP.Proxy
import System.IO

-------------------------------------
-- Core configuration variables

config "commandPrefixes"    [t| [String]                |] [| ["@", "?"]    |]
config "disabledCommands"   [t| [String]                |] [| []            |]
config "evalPrefixes"       [t| [String]                |] [| [">"]         |]
config "onStartupCmds"      [t| [String]                |] [| []            |]
config "outputDir"          [t| FilePath                |] [| "State/"      |]
config "proxy"              [t| Proxy                   |] [| NoProxy       |]

-- basic logging.  for more complex setups, configure directly using System.Log.Logger
config "replaceRootLogger"  [t| Bool                    |] [| True                        |]
config "consoleLogHandle"   [t| Handle                  |] [| stderr                      |]
config "consoleLogLevel"    [t| Priority                |] [| NOTICE                      |]
config "consoleLogFormat"   [t| String                  |] [| "[$prio] $loggername: $msg" |]

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

defaultIrcHandler :: SomeException -> IO ()
defaultIrcHandler = errorM . ("Main: caught (and ignoring) "++) . show

config "uncaughtExceptionHandler" [t| SomeException -> IO () |] [| defaultIrcHandler |]
