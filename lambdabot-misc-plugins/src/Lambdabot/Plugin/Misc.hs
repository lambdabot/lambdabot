module Lambdabot.Plugin.Misc
    ( dummyPlugin
    , errorPlugin
    , freshPlugin
    , helloPlugin
    , todoPlugin
    
    , miscPlugins
    ) where

import Lambdabot.Plugin.Misc.Dummy
import Lambdabot.Plugin.Misc.Error
import Lambdabot.Plugin.Misc.Fresh
import Lambdabot.Plugin.Misc.Hello
import Lambdabot.Plugin.Misc.Todo

miscPlugins :: [String]
miscPlugins = ["dummy", "error", "fresh", "hello", "stats", "todo"]
