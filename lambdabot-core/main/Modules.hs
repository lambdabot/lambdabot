{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Data.List
import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import qualified Lambdabot.Plugin.Activity
import qualified Lambdabot.Plugin.Base
import qualified Lambdabot.Plugin.BF
import qualified Lambdabot.Plugin.Check
import qualified Lambdabot.Plugin.Compose
import qualified Lambdabot.Plugin.Dice
import qualified Lambdabot.Plugin.Dict
import qualified Lambdabot.Plugin.Djinn
import qualified Lambdabot.Plugin.Dummy
import qualified Lambdabot.Plugin.Elite
import qualified Lambdabot.Plugin.Eval
import qualified Lambdabot.Plugin.Filter
import qualified Lambdabot.Plugin.Free
import qualified Lambdabot.Plugin.Fresh
import qualified Lambdabot.Plugin.Haddock
import qualified Lambdabot.Plugin.Help
import qualified Lambdabot.Plugin.Hoogle
import qualified Lambdabot.Plugin.Instances
import qualified Lambdabot.Plugin.IRC
import qualified Lambdabot.Plugin.Karma
import qualified Lambdabot.Plugin.Localtime
import qualified Lambdabot.Plugin.More
import qualified Lambdabot.Plugin.OEIS
import qualified Lambdabot.Plugin.OfflineRC
import qualified Lambdabot.Plugin.Pl
import qualified Lambdabot.Plugin.Pointful
import qualified Lambdabot.Plugin.Poll
import qualified Lambdabot.Plugin.Pretty
import qualified Lambdabot.Plugin.Quote
import qualified Lambdabot.Plugin.Search
import qualified Lambdabot.Plugin.Seen
import qualified Lambdabot.Plugin.Slap
import qualified Lambdabot.Plugin.Source
import qualified Lambdabot.Plugin.Spell
import qualified Lambdabot.Plugin.System
import qualified Lambdabot.Plugin.Tell
import qualified Lambdabot.Plugin.Ticker
import qualified Lambdabot.Plugin.Todo
import qualified Lambdabot.Plugin.Topic
import qualified Lambdabot.Plugin.Type
import qualified Lambdabot.Plugin.Undo
import qualified Lambdabot.Plugin.Unlambda
import qualified Lambdabot.Plugin.UnMtl
import qualified Lambdabot.Plugin.Url
import qualified Lambdabot.Plugin.Version
import qualified Lambdabot.Plugin.Vixen
import qualified Lambdabot.Plugin.Where

modulesInfo :: Modules
modulesInfo = $(modules $ nub
                    -- these must be listed first.  Maybe.  Nobody really
                    -- knows, but better to be safe than sorry.
                    [ "Base"
                    , "System"
                    , "OfflineRC"

                    -- plugins also go in this list:
                    , "Activity"
                    , "BF"
                    , "Check"
                    , "Compose"
                    , "Dice"
                    , "Dict"
                    , "Djinn"
                    , "Dummy"
                    , "Elite"
                    , "Eval"
                    , "Filter"
                    , "Free"
                    , "Fresh"
                    , "Haddock"
                    , "Help"
                    , "Hoogle"
                    , "Instances"
                    , "IRC"
                    , "Karma"
                    , "Localtime"
                    , "More"
                    , "OEIS"
                    , "Pl"
                    , "Pointful"
                    , "Poll"
                    , "Pretty"
                    , "Quote"
                    , "Search"
                    , "Seen"
                    , "Slap"
                    , "Source"
                    , "Spell"
                    , "Tell"
                    , "Ticker"
                    , "Todo"
                    , "Topic"
                    , "Type"
                    , "Undo"
                    , "Unlambda"
                    , "UnMtl"
                    , "Url"
                    , "Version"
                    , "Vixen"
                    , "Where"
                    ])
