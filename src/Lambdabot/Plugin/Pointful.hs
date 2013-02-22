-- Undo pointfree transformations. Plugin code derived from Pl.hs.
module Lambdabot.Plugin.Pointful (theModule) where

import Lambdabot.Plugin

import Lambdabot.Pointful

theModule = newModule
    { moduleCmds = return
        [ (command "pointful")
            { aliases = ["pointy","repoint","unpointless","unpl","unpf"]
            , help = say "pointful <expr>. Make code pointier."
            , process = mapM_ say . lines . pointful
            }
        ]
    }
