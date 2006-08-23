--
-- | Free theorems plugin, 
-- Andrew Bromage, 2006
-- 
module Plugin.Free where

import Plugin
import Plugin.Free.FreeTheorem

PLUGIN Free

instance Module FreeModule () where
    moduleCmds _  = ["free"]
    moduleHelp _ _= "free <id :: a -> a>. Generate theorems for free"
    process_ _ _ xs = return . (:[]) . concat . intersperse " " . lines . freeTheoremStr $ xs

