--
-- | Mess with the channel topic
--
module Plugins.Topic (theModule) where

import Lambdabot
import qualified IRC
import qualified Map as M

import Control.Monad.State (gets)
import Util                (snoc, splitFirstWord)

newtype TopicModule = TopicModule ()

theModule :: MODULE
theModule = MODULE $ TopicModule ()

instance Module TopicModule () where
  moduleHelp _ _ = return "Various commands for adjusting the channel topic\nUsage: @topic-XXX #chan <string>"
  moduleCmds   _ = return ["topic-tell",
                           "topic-cons", "topic-snoc",
                           "topic-tail", "topic-init", "topic-null"]

  process _ _ src "topic-cons" text = topicSplit (:) src text
  process _ _ src "topic-snoc" text = topicSplit snoc src text
  process _ _ src "topic-tail" chan = alterTopic src chan tail
  process _ _ src "topic-init" chan = alterTopic src chan init
  process _ _ _   "topic-null" chan = send $ IRC.setTopic chan "[]"

  process _ _ src "topic-tell" chan =
      lookupTopic chan (\maybetopic ->
        case maybetopic of
	  Just x  -> ircPrivmsg src x
	  Nothing -> ircPrivmsg src "do not know that channel")

  process _ _ src cmd _
    = ircPrivmsg src ("Bug! someone forgot the handler for \""++cmd++"\"")


topicSplit :: (String -> [String] -> [String]) -> String -> String -> LB ()
topicSplit f source cmdtext = alterTopic source chan (f topic_item)
  where 
      (chan, topic_item) = splitFirstWord cmdtext

lookupTopic :: String -> (Maybe String -> LB ()) -> LB ()
lookupTopic chan f =
  do maybetopic <- gets (\s -> M.lookup (mkCN chan) (ircChannels s))
     f maybetopic

alterTopic :: String -> String -> ([String] -> [String]) -> LB ()
alterTopic source chan f =
  let p maybetopic =
        case maybetopic of
          Just x -> case reads x of
                [(xs, "")] -> send $ IRC.setTopic chan (show $ f $ xs)
                [(xs, r)] | length r <= 2 
                  -> do ircPrivmsg source $ "ignoring bogus characters: " ++ r
                        send $ IRC.setTopic chan (show $ f $ xs)

                _ -> ircPrivmsg source 
                         "topic does not parse. topic should be of the form [\"...\",...,\"...\"]"

          Nothing -> ircPrivmsg source ("I do not know the channel " ++ chan)

        in lookupTopic chan p
