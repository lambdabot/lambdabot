{-# OPTIONS -fglasgow-exts #-}
-- 	$Id: StateModule.hs,v 1.8 2003/07/29 13:03:02 eris Exp $

module StateModule where

import IRC

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE stateModule

stateModule :: StateModule
stateModule = StateModule ()

instance Module StateModule String where
    moduleName   _ = return "state"
    moduleSticky _ = False
    moduleHelp _ _ = return "@state - we all know it's evil"
    commands     _ = return ["state"]
    moduleInit   _ = writeMS "nothing yet"
    process      _ _ target _ rest = do
       modstate <- readMS
       writeMS rest
       ircPrivmsg target modstate
