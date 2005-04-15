--
-- | Karma
--
module KarmaModule (theModule) where

import IRC
import Util (mapSerializer)
import qualified Map as M

import Data.Maybe           (fromMaybe)

newtype KarmaModule = KarmaModule ()

theModule :: MODULE
theModule = MODULE $ KarmaModule ()

type KarmaState = M.Map String Integer
type Karma m a = ModuleT KarmaState m a

instance Module KarmaModule KarmaState where
    moduleHelp _ "karma"  = return "return a person's karma value"
    moduleHelp _ "karma+" = return "increment someone's karma"
    moduleHelp _ "karma-" = return "decrement someone's karma"
    moduleHelp m _        = moduleHelp m "karma"

    moduleDefState  _ = return $ M.empty
    moduleSerialize _ = Just mapSerializer

    moduleCmds _ = return ["karma", "karma+", "karma-"]
    process      _ msg target cmd rest =
        case words rest of
	  []       -> ircPrivmsg target "I can't find the karma of nobody."
          (nick:_) -> do
              karmaFM <- readMS
              case cmd of
                 "karma"  -> getKarma target sender nick karmaFM
                 "karma+" -> incKarma target sender nick karmaFM
                 "karma-" -> decKarma target sender nick karmaFM
                 _        -> error "KarmaModule: can't happen"
	    where sender = ircNick msg

getKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
getKarma target sender nick karmaFM =
    if sender == nick then
       ircPrivmsg target $ "You have a karma of " ++ (show karma)
    else
       ircPrivmsg target $ nick ++ " has a karma of " ++ (show karma)
    where karma = fromMaybe 0 (M.lookup nick karmaFM)

incKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
incKarma target sender nick state =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else do writeMS $ M.insertWith (+) nick 1 state
            ircPrivmsg target $ nick ++ "'s karma has been incremented"

decKarma :: String -> String -> String -> KarmaState -> Karma IRC ()
decKarma target sender nick state =
    if sender == nick then
       ircPrivmsg target "You can't change your own karma, silly."
    else do writeMS $ M.insertWith (+) nick (-1) state
            ircPrivmsg target $ nick ++ "'s karma has been decremented"
