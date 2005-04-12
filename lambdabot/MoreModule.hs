
module MoreModule (theModule) where

import IRC
import Control.Monad.State

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE $ MoreModule ()

-- the @more state is handled centrally
instance Module MoreModule () where
    moduleHelp _ _ = return "@more - return more bot output"
    moduleCmds   _ = return ["more"]
    process      _ _ target _ _
      = do
        morestate <- gets ircMoreState
        ircPrivmsg target morestate
