{-# LANGUAGE TemplateHaskell #-}
--
-- | Hello world plugin
--
module Plugin.Hello (theModule) where

import Plugin

plugin "Hello"

instance Module HelloModule where
    moduleCmds = return
        [ (command "hello")
            { aliases = ["goodbye"]
            , help = say "hello/goodbye <arg>. Simplest possible plugin"
            , process = \xs -> say ("Hello world. " ++ xs)
            }
        ]

