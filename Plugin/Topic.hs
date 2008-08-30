{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | The Topic plugin is an interface for messing with the channel topic.
--   It can alter the topic in various ways and keep track of the changes.
--   The advantage of having the bot maintain the topic is that we get an
--   authoritative source for the current topic, when the IRC server decides
--   to delete it due to Network Splits.
module Plugin.Topic (theModule) where

import Plugin
import Message (Message, setTopic, Nick, readNick)
import qualified Data.Map as M

import Control.Monad.State (gets)

$(plugin "Topic")

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

  process _ msg _ "topic-cons" text = lift $ alterTopic msg chan (topic_item :)
        where (chan, topic_item) = splitFirstWord text

  process _ msg _ "topic-snoc" text = lift $ alterTopic msg chan (snoc topic_item)
        where (chan, topic_item) = splitFirstWord text

  process _ msg _ "topic-tail" chan = lift $ alterTopic msg chan tail
  process _ msg _ "topic-init" chan = lift $ alterTopic msg chan init
  process _ msg _ "topic-null" chan = lift $ send (Message.setTopic (readNick msg chan) "[]") >> return []
  process _ msg _ "topic-tell" chan = lift $ lookupTopic (readNick msg chan) $ \maybetopic -> return $
        case maybetopic of
            Just x  -> [x]
            Nothing -> ["Do not know that channel"]

------------------------------------------------------------------------

-- | 'lookupTopic' Takes a channel and a modifier function f. It then
--   proceeds to look up the channel topic for the channel given, returning
--   Just t or Nothing to the modifier function which can then decide what
--   to do with the topic
lookupTopic :: Nick                          -- ^ Channel
            -> (Maybe String -> LB [String]) -- ^ Modifier function
            -> LB [String]
lookupTopic chan f = gets (\s -> M.lookup (mkCN chan) (ircChannels s)) >>= f

-- | 'alterTopic' takes a message, a channel and an altering function.
--   Then it alters the topic in the channel by the altering function,
--   returning eventual problems back to the sender.
alterTopic :: Message a
           => a                      -- ^ Original messesg
           -> String                 -- ^ Channel name
           -> ([String] -> [String]) -- ^ Modifying function
           -> LB [String]
alterTopic msg chan f =
  let chan' = readNick msg chan
      p maybetopic =
        case maybetopic of
          Just x -> case reads x of
                [(xs, "")] -> do send $ Message.setTopic chan' (show $ f $ xs)
                                 return []
                [(xs, r)] | length r <= 2
                  -> do send $ Message.setTopic chan' (show $ f $ xs)
                        return ["ignoring bogus characters: " ++ r]

                _ -> return ["Topic does not parse. Should be of the form [\"...\",...,\"...\"]"]
          Nothing -> return ["I do not know the channel " ++ chan]
   in lookupTopic chan' p
