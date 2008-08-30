{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | Free theorems plugin
-- Andrew Bromage, 2006
module Plugin.Free where

import Plugin
import Plugin.Free.FreeTheorem
import Plugin.Type (query_ghci)

$(plugin "Free")

instance Module FreeModule () where
    moduleCmds _  = ["free"]
    moduleHelp _ _= "free <ident>. Generate theorems for free"
    process_ _ _ xs = do result <- freeTheoremStr (liftM unlines . lift . query_ghci ":t") xs
                         return . (:[]) . concat . intersperse " " . lines $ result
