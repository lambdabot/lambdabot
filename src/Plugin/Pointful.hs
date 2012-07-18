-- Undo pointfree transformations. Plugin code derived from Pl.hs.
module Plugin.Pointful (theModule) where

import Plugin

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
