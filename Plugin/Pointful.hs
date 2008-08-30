{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- Undo pointfree transformations. Plugin code derived from Pl.hs.
module Plugin.Pointful (theModule) where

import Plugin

import Lambdabot.Pointful

type PfState = ()

$(plugin "Pointful")

--type Pf = ModuleLB PfState

instance Module PointfulModule PfState where

    moduleCmds _ = ["pointful","pointy","repoint","unpointless","unpl","unpf"]

    moduleHelp _ _ = "pointful <expr>. Make code pointier."

    moduleDefState _ = return $ ()

    process _ _ _ _ rest = return (lines $ pointful rest)
