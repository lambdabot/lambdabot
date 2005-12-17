--
-- | Persistent state
-- A demo plugin
--
module Plugins.State (theModule) where

import Lambdabot
import LBState
import Serial

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE $ StateModule ()

instance Module StateModule String where
    moduleCmds      _ = ["state"]
    moduleHelp    _ _ = "@state - we all know it's evil"
    moduleDefState  _ = return "This page left blank."
    moduleSerialize _ = Just stdSerial
    process _ _ _ _ rest = do
       modstate <- withMS $ \ms writer -> writer rest >> return ms
       return [modstate]
