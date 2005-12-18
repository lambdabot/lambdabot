--
-- | Hello world plugin
--
module Plugins.Hello (theModule) where

import Lambdabot

newtype HelloModule = HelloModule ()

theModule :: MODULE
theModule = MODULE $ HelloModule ()

instance Module HelloModule where
    commands _    = ["hello","goodbye"]
    process_ _ xs = return ["Hello world. " ++ xs]

