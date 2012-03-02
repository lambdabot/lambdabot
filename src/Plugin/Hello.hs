{-# LANGUAGE TemplateHaskell #-}
--
-- | Hello world plugin
--
module Plugin.Hello where

import Plugin

plugin "Hello"

instance Module HelloModule where
    moduleCmds _ =
        [ (command "hello")
            { aliases = ["goodbye"]
            , help = say "hello/goodbye <arg>. Simplest possible plugin"
            , process = \xs -> say ("Hello world. " ++ xs)
            }
        ]

