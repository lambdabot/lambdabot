module Lambdabot.Plugin.IRC
    ( ircPlugin
    , localtimePlugin
    , logPlugin
    , topicPlugin
    
    , ircPlugins
    ) where

import Lambdabot.Plugin.IRC.IRC
import Lambdabot.Plugin.IRC.Localtime
import Lambdabot.Plugin.IRC.Log
import Lambdabot.Plugin.IRC.Topic

ircPlugins :: [String]
ircPlugins = ["irc", "localtime", "log", "topic"]
