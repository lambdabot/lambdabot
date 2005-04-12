
module TopicModule (theModule) where

import IRC
import qualified Map as M

import Control.Monad.State (gets)
import Util

newtype TopicModule = TopicModule ()

theModule :: MODULE
theModule = MODULE $ TopicModule ()

instance Module TopicModule () where
  moduleHelp _ _ = return "Various commands for adjusting the channel topic"
  moduleCmds   _ = return ["topic-tell",
                           "topic-cons", "topic-snoc",
                           "topic-tail", "topic-init"]
  process _ _ src "topic-cons" text = topicSplit (:) src text
  process _ _ src "topic-snoc" text = topicSplit snoc src text
  process _ _ src "topic-tail" chan = alterTopic src chan tail
  process _ _ src "topic-init" chan = alterTopic src chan init
  process _ _ src "topic-tell" chan =
      lookupTopic chan (\maybetopic ->
        case maybetopic of
	  Just x -> ircPrivmsg src x
	  Nothing -> ircPrivmsg src "do not know that channel")
  process _ _ src cmd _
    = ircPrivmsg src ("Bug! someone forgot the handler for \""++cmd++"\"")

topicSplit :: (String -> [String] -> [String])
	       -> String -> String -> IRC ()
topicSplit f source cmdtext = alterTopic source chan (f topic_item)
  where (chan, topic_item) = splitFirstWord cmdtext

lookupTopic :: String -> (Maybe String -> IRC () ) -> IRC ()
lookupTopic chan f =
  do maybetopic <- gets (\s -> M.lookup (mkCN chan) (ircChannels s))
     f maybetopic

alterTopic :: String -> String -> ([String] -> [String]) -> IRC ()
alterTopic source chan f =
  let p maybetopic =
        case maybetopic of
          Just x -> case reads x of
                        [(xs, "")] -> ircTopic chan (show $ f $ xs)
			[(xs, r)] | length r <= 2 
				   -> do ircPrivmsg source $
				              "ignoring bogus characters: " ++ r
					 ircTopic chan (show $ f $ xs)
			_          -> ircPrivmsg source "topic does not parse"
	  Nothing -> ircPrivmsg chan "do not know that channel"
  in lookupTopic chan p
