--
-- | Hello world plugin
--
module Plugin.Hello (theModule) where

import Plugin

theModule = newModule
    { moduleCmds = return
        [ (command "hello")
            { aliases = ["goodbye"]
            , help = say "hello/goodbye <arg>. Simplest possible plugin"
            , process = \xs -> say ("Hello world. " ++ xs)
            }
        ]
    }

