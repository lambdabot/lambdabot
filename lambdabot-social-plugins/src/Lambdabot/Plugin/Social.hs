module Lambdabot.Plugin.Social
    ( activityPlugin
    , karmaPlugin
    , pollPlugin
    , seenPlugin
    , tellPlugin
    
    , socialPlugins
    ) where

import Lambdabot.Plugin.Social.Activity
import Lambdabot.Plugin.Social.Karma
import Lambdabot.Plugin.Social.Poll
import Lambdabot.Plugin.Social.Seen
import Lambdabot.Plugin.Social.Tell

socialPlugins :: [String]
socialPlugins = ["activity", "karma", "poll", "seen", "tell"]
