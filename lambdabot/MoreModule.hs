
module MoreModule (theModule) where

import IRC
import Control.Monad.State

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE moreModule

moreModule :: MoreModule
moreModule = MoreModule ()

-- the @more state is handled centrally
instance Module MoreModule () where
    moduleName   _ = "more"
    moduleHelp _ _ = return "@more - return more bot output"
    moduleCmds   _ = return ["more"]
    process      _ _ target _ _
      = do
        morestate <- liftIRC $ ircMoreState `fmap` get
        ircPrivmsg target morestate
