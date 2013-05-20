{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Data.List
import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import Lambdabot.Plugin.Activity
import Lambdabot.Plugin.Haskell
import Lambdabot.Plugin.IRC

import Lambdabot.Plugin.BF
import Lambdabot.Plugin.Dice
import Lambdabot.Plugin.Dict
import Lambdabot.Plugin.Dummy
import Lambdabot.Plugin.Elite
import Lambdabot.Plugin.Filter
import Lambdabot.Plugin.Fresh
import Lambdabot.Plugin.Karma
import Lambdabot.Plugin.OEIS
import Lambdabot.Plugin.Poll
import Lambdabot.Plugin.Quote
import Lambdabot.Plugin.Search
import Lambdabot.Plugin.Seen
import Lambdabot.Plugin.Slap
import Lambdabot.Plugin.Spell
import Lambdabot.Plugin.Tell
import Lambdabot.Plugin.Ticker
import Lambdabot.Plugin.Todo
import Lambdabot.Plugin.Topic
import Lambdabot.Plugin.Unlambda
import Lambdabot.Plugin.Url
import Lambdabot.Plugin.Vixen
import Lambdabot.Plugin.Where

modulesInfo :: Modules
modulesInfo = $(modules (corePlugins
                    -- these must be listed first.  Maybe.  Nobody really
                    -- knows, but better to be safe than sorry.
                    ++ haskellPlugins
                    ++ ["ircPlugin", "localtimePlugin", "topicPlugin"] -- ircPlugins
                    ++
                    [ "activityPlugin"
                    , "bfPlugin"
                    , "dicePlugin"
                    , "dictPlugin"
                    , "dummyPlugin"
                    , "elitePlugin"
                    , "filterPlugin"
                    , "freshPlugin"
                    , "karmaPlugin"
                    , "morePlugin"
                    , "oeisPlugin"
                    , "pollPlugin"
                    , "quotePlugin"
                    , "searchPlugin"
                    , "seenPlugin"
                    , "slapPlugin"
                    , "spellPlugin"
                    , "tellPlugin"
                    , "tickerPlugin"
                    , "todoPlugin"
                    , "unlambdaPlugin"
                    , "urlPlugin"
                    , "vixenPlugin"
                    , "wherePlugin"
                    ]))
