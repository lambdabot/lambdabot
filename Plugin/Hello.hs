--
-- | Hello world plugin
--
module Plugin.Hello (theModule) where

import Plugin

newtype HelloModule = HelloModule ()

theModule :: MODULE
theModule = MODULE $ HelloModule ()

instance Module HelloModule where
    moduleCmds _    = ["hello","goodbye"]
    moduleHelp _    = "hello/goodbye <arg>. Simplest possible plugin" 
    process_ _ xs = return ["Hello world. " ++ xs]

