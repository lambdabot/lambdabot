module KarmaModule where

import qualified Map as M
import IRC
import Data.FiniteMap
import Control.Monad.State
import Data.IORef
import Util

newtype KarmaModule = KarmaModule ()

theModule = MODULE karmaModule
karmaModule = KarmaModule ()

instance Module KarmaModule where
    moduleName   m = return "karma"
    moduleSticky m = False
    commands     m = return ["karma", "karma+", "karma-"]
    moduleInit   m = makeInitialState "karma" (emptyFM :: FiniteMap String Integer)
    process      m msg target cmd rest =
	if (length $ words rest) == 0 then
	   ircPrivmsg target "I can't find the karma of nobody."
        else
           do let nick = head $ words rest
	      maybeKarmaFMRef <-
		  gets (\s -> M.lookup "karma" (ircModuleState s))
	      case maybeKarmaFMRef of
		  Just karmaFMRef ->
		      do karmaFMState <- liftIO $ readIORef karmaFMRef
			 let (karmaFM :: FiniteMap String Integer) =
				 stripMS karmaFMState
			 case cmd of
			    "karma"  ->
				getKarma target sender nick karmaFM
			    "karma+" ->
				incKarma target sender nick
					 karmaFMRef karmaFM
			    "karma-" ->
				decKarma target sender nick
					 karmaFMRef karmaFM
		  Nothing ->
		      do liftLB $ moduleInit m
			 process m msg target cmd rest
	    where sender = ircnick msg

getKarma target sender nick karmaFM =
    if sender == nick then
       ircPrivmsg target $ "You have a karma of " ++ (show karma)
    else
       ircPrivmsg target $ nick ++ " has a karma of " ++ (show karma)
    where karma = lookupWithDefaultFM karmaFM 0 nick

incKarma target sender nick karmaFMRef karmaFM =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else
       do liftIO . writeIORef karmaFMRef . ModuleState $
		 addToFM_C (+) karmaFM nick 1
	  ircPrivmsg target $ nick ++ "'s karma has been incremented."

decKarma target sender nick karmaFMRef karmaFM =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else
       do liftIO . writeIORef karmaFMRef . ModuleState $
		 addToFM_C (+) karmaFM nick $ -1
	  ircPrivmsg target $ nick ++ "'s karma has been decremented."
