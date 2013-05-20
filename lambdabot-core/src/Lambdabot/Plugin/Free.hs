-- | Free theorems plugin
-- Andrew Bromage, 2006
module Lambdabot.Plugin.Free (theModule) where

import Lambdabot.Plugin
import Lambdabot.Plugin.Free.FreeTheorem
import Lambdabot.Plugin.Type (query_ghci)

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command "free")
            { help = say "free <ident>. Generate theorems for free"
            , process = \xs -> do
                result <- freeTheoremStr (query_ghci ":t") xs
                say . unwords . lines $ result
            }
        ]
    }
