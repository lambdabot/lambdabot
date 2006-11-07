-- Undo pointfree transformations. Plugin code derived from Pl.hs.
module Plugin.Pointful (theModule) where

import Plugin

import Lib.Pointful

type PfState = ()

PLUGIN Pointful

--type Pf = ModuleLB PfState

instance Module PointfulModule PfState where

    moduleCmds _ = ["pointful","pointy","repoint","unpointless","unpl","unpf"]

    moduleHelp _ _ = "pointful <expr>. Make code pointier."

    moduleDefState _ = return $ ()

    process _ _ _ _ rest = case pointful rest of
                             ParseOk h -> return (lines h)
                             ParseFailed l s -> return (lines (show l ++ ": " ++ s))
