{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | The Topic plugin is an interface for messing with the channel topic.
--   It can alter the topic in various ways and keep track of the changes.
--   The advantage of having the bot maintain the topic is that we get an
--   authoritative source for the current topic, when the IRC server decides
--   to delete it due to Network Splits.
module Plugin.Topic (theModule) where

import Plugin
import Message (Message, setTopic, Nick, nName, readNick)
import qualified Data.Map as M

import Control.Monad.State (gets)

$(plugin "Topic")

type TopicAction = Nick -> String -> LB [String]
data TopicCommand = TopicCommand
        { commandAliases        :: [String]
        , commandHelp           :: String
        , invokeCommand         :: TopicAction
        }

commands :: M.Map String TopicCommand
commands = M.fromList [(alias, cmd) | cmd <- cmds, alias <- commandAliases cmd]
        where cmds =
                [ TopicCommand ["set-topic"]
                  "Set the topic of the channel, without using all that listy stuff"
                  (installTopic)
                , TopicCommand ["get-topic"]
                  "Recite the topic of the channel"
                  (tellTopic)
                  
                , TopicCommand ["shift-topic", "topic-cons", "queue-topic"]
                  "Add a new topic item to the front of the topic list"
                  (alterListTopic (:))
                , TopicCommand ["unshift-topic", "topic-tail"]
                  "Remove a topic item from the front of the topic list"
                  (alterListTopic (const tail))
                  
                , TopicCommand ["push-topic"]
                  "Add a new topic item to the end of the topic stack"
                  (alterListTopic (\arg -> (++ [arg])))
                , TopicCommand ["pop-topic", "dequeue-topic"]
                  "Pop an item from the end of the topic stack"
                  (alterListTopic (const init))
                  
                , TopicCommand ["topic-null", "clear-topic"]
                  "Empty the topic stack"
                  (alterListTopic (\_ _ -> []))
                
--                , TopicCommand ["alter-topic"]
--                  "Apply a haskell expression to the topic"
--                  (...)
                ]

instance Module TopicModule () where
  moduleHelp _ s = case words s of
          [cmd] -> "@" ++ s ++ " -- " ++ commandHelp (commands M.! cmd)
          _     -> "Are you typing with your feet?"

  moduleCmds   _ = M.keys commands

  process _ msg tgt cmd args = lift (doTopic msg tgt cmd args)


parseChan :: Message a => a -> Nick -> String -> (Maybe Nick, String)
parseChan msg tgt args = case word args of
        (cName@('#':_), rest)   -> (Just (readNick msg cName), rest)
        _                       -> case nName tgt of
                ('#':_) -> (Just tgt, args)
                _       -> (Nothing, args)

doTopic :: Message a => a -> Nick -> String -> String -> LB [String]
doTopic msg tgt cmd args = do
        let (chan, rest) = parseChan msg tgt args
        
        case chan of
                Just chan       -> invokeCommand (commands M.! cmd) chan rest
                Nothing         -> return ["What channel?"]

word :: String -> (String, String)
word str = case break isSpace (dropWhile isSpace str) of
                (w,rest) -> (w, dropWhile isSpace rest)

tellTopic chan ""       = withTopic chan $ \topic -> do
        return [nName chan ++ ": " ++ topic]
tellTopic _ ('#':_)     = return ["One channel at a time.  Jeepers!"]
tellTopic _ _           = return ["I don't know what all that extra stuff is about."]

installTopic chan topic = withTopic chan $ \_oldTopic -> do
        send (Message.setTopic chan topic)
        return []

alterTopic f chan args = withTopic chan $ \oldTopic -> do
        send (Message.setTopic chan (f args oldTopic))
        return []

alterListTopic :: (String -> [String] -> [String]) -> TopicAction
alterListTopic f = alterTopic $ \args topic -> show $ case reads topic of
        [(xs, "")]      -> f args xs
        _               -> f args [topic]

------------------------------------------------------------------------

-- | 'lookupTopic' Takes a channel and a modifier function f. It then
--   proceeds to look up the channel topic for the channel given, returning
--   Just t or Nothing to the modifier function which can then decide what
--   to do with the topic
lookupTopic :: Nick                          -- ^ Channel
            -> (Maybe String -> LB [String]) -- ^ Modifier function
            -> LB [String]
lookupTopic chan f = gets (\s -> M.lookup (mkCN chan) (ircChannels s)) >>= f

withTopic :: Nick -> (String -> LB [String]) -> LB [String]
withTopic chan f = lookupTopic chan $ \maybetopic ->
        case maybetopic of
                Just t  -> f t
                Nothing -> return ["I don't know that channel."]

-- | 'alterTopic' takes a message, a channel and an altering function.
--   Then it alters the topic in the channel by the altering function,
--   returning eventual problems back to the sender.
-- alterTopic :: Message a
--            => a                      -- ^ Original messesg
--            -> String                 -- ^ Channel name
--            -> ([String] -> [String]) -- ^ Modifying function
--            -> LB [String]
-- alterTopic msg chan f =
--   let chan' = readNick msg chan
--       p maybetopic =
--         case maybetopic of
--           Just x -> case reads x of
--                 [(xs, "")] -> do send $ Message.setTopic chan' (show $ f $ xs)
--                                  return []
--                 [(xs, r)] | length r <= 2
--                   -> do send $ Message.setTopic chan' (show $ f $ xs)
--                         return ["ignoring bogus characters: " ++ r]
-- 
--                 _ -> return ["Topic does not parse. Should be of the form [\"...\",...,\"...\"]"]
--           Nothing -> return ["I do not know the channel " ++ chan]
--    in lookupTopic chan' p
-- 