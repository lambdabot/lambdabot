--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- a Haskell evaluator for the pure part, using `plugs`
--

module PlugsModule where

import PlugsModule.RunPlugs

import IRC
import Control.Monad.Trans ( liftIO )

newtype PlugsModule = PlugsModule ()

theModule :: MODULE
theModule = MODULE plugsModule

plugsModule :: PlugsModule
plugsModule = PlugsModule ()

instance Module PlugsModule where
        moduleName   _ = return "plugs"
        moduleSticky _ = False
        commands     _ = return ["plugs"]
        process _ _ src "plugs" s = do o <- liftIO $ plugs s 
                                       ircPrivmsg src o
        process _ _ _ _ _ = error "PlugsModule: invalid command"

