-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Lambdabot version information
module Lambdabot.Plugin.Core.Version (versionPlugin) where

import Lambdabot.Plugin
import Paths_lambdabot_core (version)
import Data.Version (showVersion)

versionPlugin :: Module ()
versionPlugin = newModule
    { moduleCmds = return
        [ (command "version")
            { help = say $
                "version/source. Report the version " ++
                "and git repo of this bot"
            , process = const $ do
                say $ "lambdabot " ++ showVersion version
                say "git clone git://github.com/mokus0/lambdabot.git"
            }
        ]
    }
