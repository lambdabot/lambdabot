{-# LANGUAGE TemplateHaskell #-}
-- | The Topic plugin is an interface for messing with the channel topic.
--   It can alter the topic in various ways and keep track of the changes.
--   The advantage of having the bot maintain the topic is that we get an
--   authoritative source for the current topic, when the IRC server decides
--   to delete it due to Network Splits.
module Plugin.Topic (theModule) where

import Plugin
import Lambdabot.Message as Msg (Message, setTopic, Nick, nName, readNick)
import qualified Data.Map as M

import Control.Monad.State (gets)

$(plugin "Topic")

type TopicAction = Nick -> String -> LB [String]
data TopicCommand = TopicCommand
    { commandAliases    :: [String]
    , commandHelp       :: String
    , invokeCommand     :: TopicAction
    }

commands :: M.Map String TopicCommand
commands = M.fromList [(alias, cmd) | cmd <- cmds, alias <- commandAliases cmd]
    where cmds =
            [ TopicCommand ["set-topic"]
              "Set the topic of the channel, without using all that listy stuff"
              (installTopic)
            , TopicCommand ["get-topic"]
              "Recite the topic of the channel"
              (reciteTopic)
              
            , TopicCommand ["unshift-topic", "queue-topic"]
              "Add a new topic item to the front of the topic list"
              (alterListTopic (:))
            , TopicCommand ["shift-topic"]
              "Remove a topic item from the front of the topic list"
              (alterListTopic (const tail))
              
            , TopicCommand ["push-topic"]
              "Add a new topic item to the end of the topic stack"
              (alterListTopic (\arg -> (++ [arg])))
            , TopicCommand ["pop-topic", "dequeue-topic"]
              "Pop an item from the end of the topic stack"
              (alterListTopic (const init))
              
            , TopicCommand ["clear-topic"]
              "Empty the topic stack"
              (alterListTopic (\_ _ -> []))
            ]

instance Module TopicModule where
  moduleHelp _ s = case words s of
    [cmd] -> "@" ++ s ++ " -- " ++ commandHelp (commands M.! cmd)
    _     -> "Are you typing with your feet?"

  moduleCmds   _ = M.keys commands

  process _ msg tgt cmd args = case chan of
    Just chan   -> lift (invokeCommand (commands M.! cmd) chan rest)
    Nothing     -> return ["What channel?"]
    
    where (chan, rest) = case splitFirstWord args of
            (c@('#':_), r)  -> (Just (readNick msg c), r)
            _               -> case nName tgt of
                ('#':_)         -> (Just tgt, args)
                _               -> (Nothing, args)

------------------------------------------------------------------------
-- Topic action implementations

installTopic :: TopicAction
installTopic chan topic = withTopic chan $ \_oldTopic -> do
    send (Msg.setTopic chan topic)
    return []

reciteTopic :: TopicAction
reciteTopic chan ""       = withTopic chan $ \topic -> do
    return [nName chan ++ ": " ++ topic]
reciteTopic _ ('#':_)     = return ["One channel at a time.  Jeepers!"]
reciteTopic _ _           = return ["I don't know what all that extra stuff is about."]

alterTopic :: (String -> String -> String) -> TopicAction
alterTopic f chan args = withTopic chan $ \oldTopic -> do
    send (Msg.setTopic chan (f args oldTopic))
    return []

alterListTopic :: (String -> [String] -> [String]) -> TopicAction
alterListTopic f = alterTopic $ \args topic -> show $ case reads topic of
    [(xs, "")]  -> f args xs
    _           -> f args [topic]

------------------------------------------------------------------------

-- | 'lookupTopic' Takes a channel and a modifier function f. It then
--   proceeds to look up the channel topic for the channel given, returning
--   Just t or Nothing to the modifier function which can then decide what
--   to do with the topic
lookupTopic :: Nick                          -- ^ Channel
            -> (Maybe String -> LB [String]) -- ^ Modifier function
            -> LB [String]
lookupTopic chan f = gets (\s -> M.lookup (mkCN chan) (ircChannels s)) >>= f

-- | 'withTopic' is like 'lookupTopic' except that it ditches the Maybe in
--   favor of just yelling at the user when things don't work out as planned.
withTopic :: Nick -> (String -> LB [String]) -> LB [String]
withTopic chan f = lookupTopic chan $ \maybetopic ->
    case maybetopic of
        Just t  -> f t
        Nothing -> return ["I don't know that channel."]
