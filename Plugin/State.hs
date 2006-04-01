--
-- | Persistent state
-- A demo plugin
--
module Plugin.State (theModule) where

import Plugin

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE $ StateModule ()

instance Module StateModule String where
    moduleCmds      _ = ["state"]
    moduleHelp    _ _ = "state [expr]. Get or set a state variable."
    moduleDefState  _ = return "This page intentionally left blank."
    moduleSerialize _ = Just stdSerial
    process_ _ _ rest = do m <- withMS $ flip ((>>) . ($ rest)) . return
                           return [m]
