module Lambdabot.Plugin.Reference
    ( dictPlugin
    , metarPlugin
    , oeisPlugin
    , searchPlugin
    , spellPlugin
    , tickerPlugin
    , urlPlugin
    , wherePlugin
    
    , referencePlugins
    
    , module Lambdabot.Config.Reference
    ) where

import Lambdabot.Config.Reference
import Lambdabot.Plugin.Reference.Dict
import Lambdabot.Plugin.Reference.Metar
import Lambdabot.Plugin.Reference.OEIS
import Lambdabot.Plugin.Reference.Search
import Lambdabot.Plugin.Reference.Spell
import Lambdabot.Plugin.Reference.Ticker
import Lambdabot.Plugin.Reference.Url
import Lambdabot.Plugin.Reference.Where

referencePlugins :: [String]
referencePlugins = ["dict", "metar", "oeis", "search", "spell", "ticker", "url", "where"]
