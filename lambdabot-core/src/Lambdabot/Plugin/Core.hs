module Lambdabot.Plugin.Core
    ( basePlugin
    , systemPlugin
    , offlineRCPlugin
    , composePlugin
    , helpPlugin
    , morePlugin
    , versionPlugin
    
    , corePlugins
    , module Lambdabot.Config.Core
    ) where

import Lambdabot.Config.Core
import Lambdabot.Plugin.Core.Base
import Lambdabot.Plugin.Core.Compose
import Lambdabot.Plugin.Core.Help
import Lambdabot.Plugin.Core.More
import Lambdabot.Plugin.Core.OfflineRC
import Lambdabot.Plugin.Core.System
import Lambdabot.Plugin.Core.Version

corePlugins :: [String]
corePlugins = ["base", "system", "offlineRC", "compose", "help", "more", "version"]
