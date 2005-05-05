--
-- | Persistent state
--
module Plugins.State (theModule) where

import Lambdabot
import Util (stdSerializer)

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE $ StateModule ()

instance Module StateModule String where
    moduleHelp    _ _ = return "@state - we all know it's evil"
    moduleCmds      _ = return ["state"]
    moduleDefState  _ = return "nothing yet"
    moduleSerialize _ = Just stdSerializer
    process      _ _ target _ rest = do
       modstate <- withMS $ \ms writer -> writer rest >> return ms
       ircPrivmsg target modstate
