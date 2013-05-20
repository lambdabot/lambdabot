--
-- | Hello world plugin
--
module Lambdabot.Plugin.Hello (theModule) where

import Lambdabot.Plugin

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command "hello")
            { aliases = ["goodbye"]
            , help = say "hello/goodbye <arg>. Simplest possible plugin"
            , process = \xs -> say ("Hello world. " ++ xs)
            }
        ]
    }

