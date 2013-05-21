module Lambdabot.Plugin.Novelty 
    ( bfPlugin
    , dicePlugin
    , elitePlugin
    , filterPlugin
    , numberwangPlugin
    , quotePlugin
    , slapPlugin
    , unlambdaPlugin
    , vixenPlugin
    
    , noveltyPlugins
    , module Lambdabot.Config.Novelty
    ) where

import Lambdabot.Config.Novelty
import Lambdabot.Plugin.Novelty.BF
import Lambdabot.Plugin.Novelty.Dice
import Lambdabot.Plugin.Novelty.Elite
import Lambdabot.Plugin.Novelty.Filter
import Lambdabot.Plugin.Novelty.Numberwang
import Lambdabot.Plugin.Novelty.Quote
import Lambdabot.Plugin.Novelty.Slap
import Lambdabot.Plugin.Novelty.Unlambda
import Lambdabot.Plugin.Novelty.Vixen

noveltyPlugins :: [String]
noveltyPlugins = ["bfPlugin", "dicePlugin", "elitePlugin", "filterPlugin", "numberwangPlugin",
                  "quotePlugin", "slapPlugin", "unlambdaPlugin", "vixenPlugin"]
