-- | Free theorems plugin
-- Andrew Bromage, 2006
module Lambdabot.Plugin.Haskell.Free (freePlugin) where

import Lambdabot.Plugin
import Lambdabot.Plugin.Haskell.Free.FreeTheorem
import Lambdabot.Plugin.Haskell.Type (query_ghci)

freePlugin :: Module ()
freePlugin = newModule
    { moduleCmds = return
        [ (command "free")
            { help = say "free <ident>. Generate theorems for free"
            , process = \xs -> do
                result <- freeTheoremStr (query_ghci ":t") xs
                say . unwords . lines $ result
            }
        ]
    }
