{-# OPTIONS -fglasgow-exts #-}

module KarmaModule where

import IRC
import Data.FiniteMap
import Control.Monad.Reader
-- import Util

newtype KarmaModule = KarmaModule ()

theModule :: MODULE
theModule = MODULE karmaModule

karmaModule :: KarmaModule
karmaModule = KarmaModule ()

-- TODO: Port to Map
type KarmaState = FiniteMap String Integer
type Karma = ModuleT KarmaState

instance Module KarmaModule KarmaState where
    moduleName   _ = return "karma"

    moduleHelp _ "karma"  = return "return a person's karma value"
    moduleHelp _ "karma+" = return "increment someone's karma"
    moduleHelp _ "karma-" = return "decrement someone's karma"
    moduleHelp m _        = moduleHelp m "karma" 

    moduleSticky _ = False
    commands     _ = return ["karma", "karma+", "karma-"]
    moduleInit   _ = lift $ makeInitialState "karma" (emptyFM :: FiniteMap String Integer)
    process      _ msg target cmd rest =
	if (length $ words rest) == 0 then
	   ircPrivmsg target "I can't find the karma of nobody."
        else
           do let nick = head $ words rest
              karmaFM <- readMS
              case cmd of
                 "karma"  -> getKarma target sender nick karmaFM
                 "karma+" -> incKarma target sender nick karmaFM
                 "karma-" -> decKarma target sender nick karmaFM
                 _        -> error "KarmaModule: can't happen"
	    where sender = ircnick msg

getKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
getKarma target sender nick karmaFM =
    if sender == nick then
       ircPrivmsg target $ "You have a karma of " ++ (show karma)
    else
       ircPrivmsg target $ nick ++ " has a karma of " ++ (show karma)
    where karma = lookupWithDefaultFM karmaFM 0 nick

incKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
incKarma target sender nick karmaFM =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else
       do writeMS $ addToFM_C (+) karmaFM nick 1
	  ircPrivmsg target $ nick ++ "'s karma has been incremented."

decKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
decKarma target sender nick karmaFM =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else
       do writeMS $ addToFM_C (+) karmaFM nick $ -1
	  ircPrivmsg target $ nick ++ "'s karma has been decremented."
