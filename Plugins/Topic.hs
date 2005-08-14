--
-- | The Topic plugin is an interface for messing with the channel topic.
--   It can alter the topic in various ways and keep track of the changes.
--   The advantage of having the bot maintain the topic is that we get an
--   authoritative source for the current topic, when the IRC server decides
--   to delete it due to Network Splits.
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
  moduleHelp _ _ = return $ concat ["Various commands for adjusting the channel topic\n",
				    "Usage: @topic-XXX #chan <string>"]
  moduleCmds   _ = return ["topic-tell",
                           "topic-cons", "topic-snoc",
                           "topic-tail", "topic-init", "topic-null"]

  process _ _ src "topic-cons" text =
      alterTopic src chan (topic_item :)
	  where (chan, topic_item) = splitFirstWord text
  process _ _ src "topic-snoc" text =
      alterTopic src chan (snoc topic_item)
          where (chan, topic_item) = splitFirstWord text

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

-- | 'lookupTopic' Takes a channel and a modifier function f. It then
--   proceeds to look up the channel topic for the channel given, returning
--   Just t or Nothing to the modifier function which can then decide what
--   to do with the topic
lookupTopic :: String -- ^ Channel
	    -> (Maybe String -> LB ()) -- ^ Modifier function
	    -> LB ()
lookupTopic chan f =
  do maybetopic <- gets (\s -> M.lookup (mkCN chan) (ircChannels s))
     f maybetopic

-- | 'alterTopic' takes a sender, a channel and an altering function.
--   then it alters the topic in the channel by the altering function,
--   returning eventual problems back to the sender.
alterTopic :: String                 -- ^ Sender
	   -> String                 -- ^ Channel
	   -> ([String] -> [String]) -- ^ Modifying function
	   -> LB ()
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
