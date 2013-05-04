{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Lambdabot.Plugin.Error (theModule, failOnLoad, errorOnLoad) where

import Lambdabot.Config
import Lambdabot.Plugin

import Control.Monad

config "failOnLoad"  [t| Bool |] [| False |]
config "errorOnLoad" [t| Bool |] [| False |]

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command "error")
            { help = say "Throw an error, see what lambdabot does with it!"
            , process = error . show
            }
        , (command "fail")
            { help = say "Throw an error, see what lambdabot does with it!"
            , process = fail . show
            }
        ]
    , moduleInit = do
        shouldFail <- getConfig failOnLoad
        when shouldFail (fail "Error module hates the world!")
        
        shouldError <- getConfig errorOnLoad
        when shouldError (error "Error module hates the world!")
    }
