--
-- | Persistent state
-- A demo plugin
--
module Plugin.State (theModule) where

import Lambdabot
import LBState
import Serial

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE $ StateModule ()

instance Module StateModule String where
    moduleCmds      _ = ["state"]
    moduleHelp    _ _ = "state [expr]. Get or set a state variable."
    moduleDefState  _ = return "This page intentionally left blank."
    moduleSerialize _ = Just stdSerial
    process_ _ _ rest = do modstate <- withMS $ \ms writer -> writer rest >> return ms
                           return [modstate]
