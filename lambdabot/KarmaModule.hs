{-# OPTIONS -fglasgow-exts #-}

module KarmaModule where

import qualified Map as M
import IRC
import Data.FiniteMap
import Control.Monad.State
import Data.IORef
-- import Util
import Data.Typeable (Typeable)

newtype KarmaModule = KarmaModule ()

theModule :: MODULE
theModule = MODULE karmaModule

karmaModule :: KarmaModule
karmaModule = KarmaModule ()

instance Module KarmaModule where
    moduleName   _ = return "karma"
    moduleSticky _ = False
    commands     _ = return ["karma", "karma+", "karma-"]
    moduleInit   _ = makeInitialState "karma" (emptyFM :: FiniteMap String Integer)
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
                            _ -> error "KarmaModule: can't happen"
		  Nothing ->
		      do liftLB $ moduleInit m
			 process m msg target cmd rest
	    where sender = ircnick msg

getKarma :: (Num elt) => String -> [Char] -> [Char] -> FiniteMap [Char] elt -> IRC ()
getKarma target sender nick karmaFM =
    if sender == nick then
       ircPrivmsg target $ "You have a karma of " ++ (show karma)
    else
       ircPrivmsg target $ nick ++ " has a karma of " ++ (show karma)
    where karma = lookupWithDefaultFM karmaFM 0 nick

incKarma :: (Num elt, Typeable (FiniteMap [Char] elt)) =>
	    String
	    -> [Char]
	       -> [Char] -> IORef ModuleState -> FiniteMap [Char] elt -> IRC ()
incKarma target sender nick karmaFMRef karmaFM =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else
       do liftIO . writeIORef karmaFMRef . ModuleState $
		 addToFM_C (+) karmaFM nick 1
	  ircPrivmsg target $ nick ++ "'s karma has been incremented."

decKarma :: (Num a, Typeable (FiniteMap [Char] a)) =>
	    String
	    -> [Char]
	       -> [Char] -> IORef ModuleState -> FiniteMap [Char] a -> IRC ()
decKarma target sender nick karmaFMRef karmaFM =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else
       do liftIO . writeIORef karmaFMRef . ModuleState $
		 addToFM_C (+) karmaFM nick $ -1
	  ircPrivmsg target $ nick ++ "'s karma has been decremented."
