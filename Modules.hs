{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Plugin
import Data.List (nub)

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import qualified Plugin.Activity
import qualified Plugin.Babel
import qualified Plugin.Base
import qualified Plugin.BF
import qualified Plugin.Check
import qualified Plugin.Compose
import qualified Plugin.Dice
import qualified Plugin.Dict
import qualified Plugin.Djinn
import qualified Plugin.Dummy
import qualified Plugin.Elite
import qualified Plugin.Eval
import qualified Plugin.Fact
import qualified Plugin.Free
import qualified Plugin.Fresh
import qualified Plugin.FT
import qualified Plugin.Haddock
import qualified Plugin.Help
import qualified Plugin.Hoogle
import qualified Plugin.Instances
import qualified Plugin.IRC
import qualified Plugin.Karma
import qualified Plugin.Localtime
import qualified Plugin.More
import qualified Plugin.OEIS
import qualified Plugin.OfflineRC
import qualified Plugin.Pl
import qualified Plugin.Pointful
import qualified Plugin.Poll
import qualified Plugin.Pretty
import qualified Plugin.Quote
import qualified Plugin.Search
--import qualified Plugin.Seen
import qualified Plugin.Slap
import qualified Plugin.Source
import qualified Plugin.Spell
import qualified Plugin.State
import qualified Plugin.System
import qualified Plugin.Tell
import qualified Plugin.Ticker
import qualified Plugin.Todo
import qualified Plugin.Topic
import qualified Plugin.Type
import qualified Plugin.Undo
import qualified Plugin.Unlambda
import qualified Plugin.UnMtl
--import qualified Plugin.Url
import qualified Plugin.Version
import qualified Plugin.Vixen
import qualified Plugin.Where

modulesInfo :: (LB (), [String])
modulesInfo = $(modules $ nub
                    -- these must be listed first.  Maybe.  Nobody really
                    -- knows, but better to be safe than sorry.
                    [ "Base"
                    , "State"
                    , "System"
                    , "OfflineRC"

                    -- plugins also go in this list:
                    , "Activity"
                    , "Babel"
                    , "BF"
                    , "Check"
                    , "Compose"
                    , "Dice"
                    , "Dict"
                    , "Djinn"
                    , "Dummy"
                    , "Elite"
                    , "Eval"
                    , "Fact"
                    , "Free"
                    , "Fresh"
                    , "FT"
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
                    --, "Seen"
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
                    --, "Url"
                    , "Version"
                    , "Vixen"
                    , "Where"
                    ])
