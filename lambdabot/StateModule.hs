
module StateModule (theModule) where

import IRC

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE stateModule

stateModule :: StateModule
stateModule = StateModule ()

instance Module StateModule String where
    moduleName   _ = return "state"
    moduleHelp _ _ = return "@state - we all know it's evil"
    commands     _ = return ["state"]
    moduleInit   _ = writeMS "nothing yet"
    process      _ _ target _ rest = do
       modstate <- readMS
       writeMS rest
       ircPrivmsg target modstate
