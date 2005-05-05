--
-- | Karma
--
module Plugins.Karma (theModule) where

import Lambdabot
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
              case cmd of
                 "karma"  -> getKarma         target sender nick
                 "karma+" -> changeKarma 1    target sender nick
                 "karma-" -> changeKarma (-1) target sender nick
                 _        -> error "KarmaModule: can't happen"
	    where sender = ircNick msg

getKarma :: String -> String -> String -> Karma IRC ()
getKarma target sender nick = do
    karmaFM <- readMS
    let karma = fromMaybe 0 (M.lookup nick karmaFM)
    ircPrivmsg target $ (if sender == nick then "You have" else nick ++ " has")
      ++ " a karma of " ++ show karma

changeKarma :: Integer -> String -> String -> String -> Karma IRC ()
changeKarma km target sender nick
  | sender == nick = ircPrivmsg target "You can't change your own karma, silly."
  | otherwise      = do
      modifyMS $ M.insertWith (+) nick km
      ircPrivmsg target $ nick ++ "'s karma has been incremented"
