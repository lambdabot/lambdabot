{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Data.List
import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import Lambdabot.Plugin.Activity
import Lambdabot.Plugin.Base
import Lambdabot.Plugin.BF
import Lambdabot.Plugin.Check
import Lambdabot.Plugin.Compose
import Lambdabot.Plugin.Dice
import Lambdabot.Plugin.Dict
import Lambdabot.Plugin.Djinn
import Lambdabot.Plugin.Dummy
import Lambdabot.Plugin.Elite
import Lambdabot.Plugin.Eval
import Lambdabot.Plugin.Filter
import Lambdabot.Plugin.Free
import Lambdabot.Plugin.Fresh
import Lambdabot.Plugin.Haddock
import Lambdabot.Plugin.Help
import Lambdabot.Plugin.Hoogle
import Lambdabot.Plugin.Instances
import Lambdabot.Plugin.IRC
import Lambdabot.Plugin.Karma
import Lambdabot.Plugin.Localtime
import Lambdabot.Plugin.More
import Lambdabot.Plugin.OEIS
import Lambdabot.Plugin.OfflineRC
import Lambdabot.Plugin.Pl
import Lambdabot.Plugin.Pointful
import Lambdabot.Plugin.Poll
import Lambdabot.Plugin.Pretty
import Lambdabot.Plugin.Quote
import Lambdabot.Plugin.Search
import Lambdabot.Plugin.Seen
import Lambdabot.Plugin.Slap
import Lambdabot.Plugin.Source
import Lambdabot.Plugin.Spell
import Lambdabot.Plugin.System
import Lambdabot.Plugin.Tell
import Lambdabot.Plugin.Ticker
import Lambdabot.Plugin.Todo
import Lambdabot.Plugin.Topic
import Lambdabot.Plugin.Type
import Lambdabot.Plugin.Undo
import Lambdabot.Plugin.Unlambda
import Lambdabot.Plugin.UnMtl
import Lambdabot.Plugin.Url
import Lambdabot.Plugin.Version
import Lambdabot.Plugin.Vixen
import Lambdabot.Plugin.Where

modulesInfo :: Modules
modulesInfo = $(modules $ nub
                    -- these must be listed first.  Maybe.  Nobody really
                    -- knows, but better to be safe than sorry.
                    [ "base"
                    , "system"
                    , "offlineRC"

                    -- plugins also go in this list:
                    , "activityPlugin"
                    , "bfPlugin"
                    , "checkPlugin"
                    , "composePlugin"
                    , "dicePlugin"
                    , "dictPlugin"
                    , "djinnPlugin"
                    , "dummyPlugin"
                    , "elitePlugin"
                    , "evalPlugin"
                    , "filterPlugin"
                    , "freePlugin"
                    , "freshPlugin"
                    , "haddockPlugin"
                    , "helpPlugin"
                    , "hooglePlugin"
                    , "instancesPlugin"
                    , "ircPlugin"
                    , "karmaPlugin"
                    , "localtimePlugin"
                    , "morePlugin"
                    , "oeisPlugin"
                    , "plPlugin"
                    , "pointfulPlugin"
                    , "pollPlugin"
                    , "prettyPlugin"
                    , "quotePlugin"
                    , "searchPlugin"
                    , "seenPlugin"
                    , "slapPlugin"
                    , "sourcePlugin"
                    , "spellPlugin"
                    , "tellPlugin"
                    , "tickerPlugin"
                    , "todoPlugin"
                    , "topicPlugin"
                    , "typePlugin"
                    , "undoPlugin"
                    , "unlambdaPlugin"
                    , "unmtlPlugin"
                    , "urlPlugin"
                    , "versionPlugin"
                    , "vixenPlugin"
                    , "wherePlugin"
                    ])
