
-- 	$Id: MoreModule.hs,v 1.2 2003/07/25 13:19:22 eleganesh Exp $

module MoreModule where

import IRC
import Control.Monad.State

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE moreModule

moreModule :: MoreModule
moreModule = MoreModule ()

-- the @more state is handled centrally
instance Module MoreModule () where
    moduleName   _ = return "more"
    moduleSticky _ = False
    moduleHelp _ _ = return "@more - return more bot output"
    commands     _ = return ["more"]
    process      _ _ target _ _
      = do
        morestate <- liftIRC $ ircMoreState `fmap` get
        ircPrivmsg target morestate
