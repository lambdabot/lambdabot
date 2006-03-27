--
-- | Hello world plugin
--
module Plugins.Hello (theModule) where

import Lambdabot

newtype HelloModule = HelloModule ()

theModule :: MODULE
theModule = MODULE $ HelloModule ()

instance Module HelloModule where
    moduleCmds _    = ["hello","goodbye"]
    moduleHelp _    = "hello/goodbye <arg>. Simplest possible plugin" 
    process_ _ xs = return ["Hello world. " ++ xs]

