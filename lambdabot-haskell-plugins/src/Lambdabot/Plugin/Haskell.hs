module Lambdabot.Plugin.Haskell
    ( checkPlugin
    , djinnPlugin
    , evalPlugin
    , freePlugin
    , haddockPlugin
    , hooglePlugin
    , instancesPlugin
    , plPlugin
    , pointfulPlugin
    , prettyPlugin
    , sourcePlugin
    , typePlugin
    , undoPlugin
    , unmtlPlugin
    
    , haskellPlugins
    
    , module Lambdabot.Config.Haskell
    ) where

import Lambdabot.Config.Haskell
import Lambdabot.Plugin.Haskell.Check
import Lambdabot.Plugin.Haskell.Djinn
import Lambdabot.Plugin.Haskell.Eval
import Lambdabot.Plugin.Haskell.Free
import Lambdabot.Plugin.Haskell.Haddock
import Lambdabot.Plugin.Haskell.Hoogle
import Lambdabot.Plugin.Haskell.Instances
import Lambdabot.Plugin.Haskell.Pl
import Lambdabot.Plugin.Haskell.Pointful
import Lambdabot.Plugin.Haskell.Pretty
import Lambdabot.Plugin.Haskell.Source
import Lambdabot.Plugin.Haskell.Type
import Lambdabot.Plugin.Haskell.Undo
import Lambdabot.Plugin.Haskell.UnMtl

haskellPlugins :: [String]
haskellPlugins = ["check", "djinn", "eval", "free", "haddock", "hoogle", "instances",
                  "pl", "pointful", "pretty", "source", "type", "undo", "unmtl"]
