{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- Undo pointfree transformations. Plugin code derived from Pl.hs.
module Plugin.Pointful (theModule) where

import Plugin

import Lambdabot.Pointful

type PfState = ()

$(plugin "Pointful")

instance Module PointfulModule where
    type ModuleState PointfulModule = PfState

    moduleCmds _ =
        [ (command "pointful")
            { aliases = ["pointy","repoint","unpointless","unpl","unpf"]
            , help = say "pointful <expr>. Make code pointier."
            , process = mapM_ say . lines . pointful
            }
        ]

    moduleDefState _ = return $ ()
