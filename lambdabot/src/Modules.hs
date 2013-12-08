{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import Lambdabot.Plugin.Haskell
import Lambdabot.Plugin.IRC
import Lambdabot.Plugin.Misc
import Lambdabot.Plugin.Novelty
import Lambdabot.Plugin.Reference
import Lambdabot.Plugin.Social

modulesInfo :: Modules
modulesInfo = $(modules $ corePlugins
    ++ haskellPlugins
    ++ ["irc", "localtime", "topic"] -- ircPlugins
    ++ ["dummy", "fresh", "todo"] -- miscPlugins
    ++ ["bf", "dice", "elite", "filter", "quote", "slap", "unlambda"] -- noveltyPlugins
    ++ ["dict", "metar", "oeis", "search", "spell", "ticker", "where"] -- referencePlugins
    ++ ["activity", "karma", "poll", "tell"]) -- socialPlugins
