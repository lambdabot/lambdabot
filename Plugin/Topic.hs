--
-- | The Topic plugin is an interface for messing with the channel topic.
--   It can alter the topic in various ways and keep track of the changes.
--   The advantage of having the bot maintain the topic is that we get an
--   authoritative source for the current topic, when the IRC server decides
--   to delete it due to Network Splits.
--
module Plugin.Topic (theModule) where

import Plugin
import qualified IRC
import qualified Data.Map as M

import Control.Monad.State (gets)

PLUGIN Topic

instance Module TopicModule () where
  moduleHelp _ s = case s of
      "topic-tell" -> "@topic-tell #chan -- " ++
                       "Tell the requesting person of the topic of the channel"
      "topic-cons" -> "@topic-cons #chan <mess> -- " ++
                       "Add a new topic item to the front of the topic list"
      "topic-snoc" -> "@topic-snoc #chan <mess> -- " ++
                       "Add a new topic item to the back of the topic list"
      "topic-tail" -> "@topic-tail #chan -- " ++
                       "Remove the first topic item from the topic list"
      "topic-null" -> "@topic-null #chan -- " ++
                       "Clear out the topic entirely"
      "topic-init" -> "@topic-init #chan -- " ++
                       "Remove the last topic item from the topic list"
      _ -> "Someone forgot to document his new Topic function! Shame on him/her!"

  moduleCmds   _ = ["topic-tell",
                   "topic-cons", "topic-snoc",
                   "topic-tail", "topic-init", "topic-null"]

  process_ _ "topic-cons" text = alterTopic chan (topic_item :)
        where (chan, topic_item) = splitFirstWord text

  process_ _ "topic-snoc" text = alterTopic chan (snoc topic_item)
        where (chan, topic_item) = splitFirstWord text

  process_ _ "topic-tail" chan = alterTopic chan tail
  process_ _ "topic-init" chan = alterTopic chan init
  process_ _ "topic-null" chan = send (Just (IRC.setTopic chan "[]")) >> return []
  process_ _ "topic-tell" chan = lookupTopic chan $ \maybetopic -> return $
        case maybetopic of
            Just x  -> [x]
            Nothing -> ["Do not know that channel"]

------------------------------------------------------------------------

-- | 'lookupTopic' Takes a channel and a modifier function f. It then
--   proceeds to look up the channel topic for the channel given, returning
--   Just t or Nothing to the modifier function which can then decide what
--   to do with the topic
lookupTopic :: String                        -- ^ Channel
            -> (Maybe String -> LB [String]) -- ^ Modifier function
            -> LB [String]
lookupTopic chan f = gets (\s -> M.lookup (mkCN chan) (ircChannels s)) >>= f

-- | 'alterTopic' takes a sender, a channel and an altering function.
--   Then it alters the topic in the channel by the altering function,
--   returning eventual problems back to the sender.
alterTopic :: String                 -- ^ Channel
           -> ([String] -> [String]) -- ^ Modifying function
           -> LB [String]
alterTopic chan f =
  let p maybetopic =
        case maybetopic of
          Just x -> case reads x of
                [(xs, "")] -> do send . Just $ IRC.setTopic chan (show $ f $ xs)
                                 return []
                [(xs, r)] | length r <= 2
                  -> do send . Just $ IRC.setTopic chan (show $ f $ xs)
                        return ["ignoring bogus characters: " ++ r]

                _ -> return ["Topic does not parse. Should be of the form [\"...\",...,\"...\"]"]
          Nothing -> return ["I do not know the channel " ++ chan]
   in lookupTopic chan p
