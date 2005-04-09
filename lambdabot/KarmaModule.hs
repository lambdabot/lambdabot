module KarmaModule (theModule) where

import IRC
import Util (mapSerializer)
import qualified Map as Map

import Data.Maybe           (fromMaybe)

newtype KarmaModule = KarmaModule ()

theModule :: MODULE
theModule = MODULE $ KarmaModule ()

type KarmaState = Map.Map String Integer
type Karma = ModuleT KarmaState

instance Module KarmaModule KarmaState where
    moduleName   _ = "karma"

    moduleHelp _ "karma"  = return "return a person's karma value"
    moduleHelp _ "karma+" = return "increment someone's karma"
    moduleHelp _ "karma-" = return "decrement someone's karma"
    moduleHelp m _        = moduleHelp m "karma" 

    moduleDefState  _ = return $ Map.empty
    moduleSerialize _ = Just mapSerializer

    moduleCmds _ = return ["karma", "karma+", "karma-"]
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
    where karma = fromMaybe 0 (Map.lookup nick karmaFM)

incKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
incKarma target sender nick state =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else do writeMS $ Map.insertWith (+) nick 1 state
            ircPrivmsg target $ nick ++ "'s karma has been incremented"

decKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
decKarma target sender nick state =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else do writeMS $ Map.insertWith (+) nick (-1) state
            ircPrivmsg target $ nick ++ "'s karma has been decremented"
