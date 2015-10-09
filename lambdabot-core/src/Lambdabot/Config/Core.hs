{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Config.Core
    ( commandPrefixes
    , disabledCommands
    , editDistanceLimit
    , enableInsults
    , onStartupCmds
    , outputDir
    , dataDir
    , lbVersion
    , textWidth
    , uncaughtExceptionHandler

    , replaceRootLogger
    , lbRootLoggerPath
    , consoleLogHandle
    , consoleLogLevel
    , consoleLogFormat
    , readState
    , writeState
    , saveOnModify
    ) where

import Lambdabot.Config
import Lambdabot.Logging
import qualified Data.ByteString.Char8 as P

import Control.Exception
import System.IO
import Data.Version

-------------------------------------
-- Core configuration variables

config "commandPrefixes"    [t| [String]                |] [| ["@", "?"]    |]
config "disabledCommands"   [t| [String]                |] [| []            |]
config "editDistanceLimit"  [t| Int                     |] [| 3 :: Int      |]
config "enableInsults"      [t| Bool                    |] [| True          |]
configWithMerge [| (++) |] "onStartupCmds" [t| [String] |] [| ["offline"]   |]
config "outputDir"          [t| FilePath                |] [| "State/"      |]
-- the dataDir variable will be filled by lambdabot's executable
config "dataDir"            [t| FilePath                |] [| "."           |]
-- ditto for lbVersion
config "lbVersion"          [t| Version                 |] [| Version [] [] |]

-- IRC maximum msg length, minus a bit for safety.
config "textWidth"          [t| Int                     |] [| 200 :: Int    |]

-- basic logging.  for more complex setups, configure directly using System.Log.Logger
config "replaceRootLogger"  [t| Bool                    |] [| True                        |]
config "lbRootLoggerPath"   [t| [String]                |] [| []                          |]
config "consoleLogHandle"   [t| Handle                  |] [| stderr                      |]
config "consoleLogLevel"    [t| Priority                |] [| NOTICE                      |]
config "consoleLogFormat"   [t| String                  |] [| "[$prio] $loggername: $msg" |]
-- hooks into state modification
config "writeState"         [t| Maybe (String -> P.ByteString -> IO ()) |] [| Nothing |]
config "readState"          [t| Maybe (String -> IO (Maybe P.ByteString)) |] [| Nothing |]
config "saveOnModify"       [t| Bool |] [| False |]

--------------------------------------------
-- Default values with longer definitions

defaultIrcHandler :: SomeException -> IO ()
defaultIrcHandler = errorM . ("Main: caught (and ignoring) "++) . show

config "uncaughtExceptionHandler" [t| SomeException -> IO () |] [| defaultIrcHandler |]
