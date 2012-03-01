{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Lambdabot version information
module Plugin.Version where

import Plugin
import Paths_lambdabot (version)
import Data.Version (showVersion)

$(plugin "Version")

instance Module VersionModule where
    moduleCmds _ =
        [ (command "version")
            { help = say $
                "version/source. Report the version " ++
                "and darcs repo of this bot"
            , process = const $ do
                say $ "lambdabot " ++ showVersion version
                say "darcs get http://code.haskell.org/lambdabot" 
            }
        ]
