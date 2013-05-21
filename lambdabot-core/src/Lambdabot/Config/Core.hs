{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Core
    ( commandPrefixes
    , disabledCommands
    , onStartupCmds
    , outputDir
    , uncaughtExceptionHandler
    
    , replaceRootLogger
    , lbRootLoggerPath
    , consoleLogHandle
    , consoleLogLevel
    , consoleLogFormat
    ) where

import Lambdabot.Config
import Lambdabot.Logging

import Control.Exception
import System.IO

-------------------------------------
-- Core configuration variables

config "commandPrefixes"    [t| [String]                |] [| ["@", "?"]    |]
config "disabledCommands"   [t| [String]                |] [| []            |]
configWithMerge [| (++) |] "onStartupCmds" [t| [String] |] [| ["offline"]   |]
config "outputDir"          [t| FilePath                |] [| "State/"      |]

-- basic logging.  for more complex setups, configure directly using System.Log.Logger
config "replaceRootLogger"  [t| Bool                    |] [| True                        |]
config "lbRootLoggerPath"   [t| [String]                |] [| []                          |]
config "consoleLogHandle"   [t| Handle                  |] [| stderr                      |]
config "consoleLogLevel"    [t| Priority                |] [| NOTICE                      |]
config "consoleLogFormat"   [t| String                  |] [| "[$prio] $loggername: $msg" |]

--------------------------------------------
-- Default values with longer definitions

defaultIrcHandler :: SomeException -> IO ()
defaultIrcHandler = errorM . ("Main: caught (and ignoring) "++) . show

config "uncaughtExceptionHandler" [t| SomeException -> IO () |] [| defaultIrcHandler |]
