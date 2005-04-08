
module StateModule (theModule) where

import IRC
import Util (stdSerializer)

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE $ StateModule ()

instance Module StateModule String where
    moduleName      _ = "state"
    moduleHelp    _ _ = return "@state - we all know it's evil"
    moduleCmds      _ = return ["state"]
    moduleDefState  _ = return "nothing yet"
    moduleSerialize _ = Just stdSerializer
    process      _ _ target _ rest = do
       modstate <- readMS
       writeMS rest
       ircPrivmsg target modstate
