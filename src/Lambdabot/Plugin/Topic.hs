-- | The Topic plugin is an interface for messing with the channel topic.
--   It can alter the topic in various ways and keep track of the changes.
--   The advantage of having the bot maintain the topic is that we get an
--   authoritative source for the current topic, when the IRC server decides
--   to delete it due to Network Splits.
module Lambdabot.Plugin.Topic (theModule) where

import Lambdabot.Plugin
import Lambdabot.Monad
import Lambdabot.IRC

import qualified Data.Map as M

import Control.Monad.State (gets)

type Topic = ModuleT () LB

type TopicAction = Nick -> String -> Cmd Topic ()
data TopicCommand = TopicCommand
    { _commandAliases    :: [String]
    , _commandHelp       :: String
    , _invokeCommand     :: TopicAction
    }

commands :: [TopicCommand]
commands =
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

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command name)
            { help = say helpStr
            , aliases = aliases'
            , process = \args -> do
                tgt <- getTarget
                (chan, rest) <- case splitFirstWord args of
                        (c@('#':_), r)  -> do
                            c' <- readNick c
                            return (Just c', r)
                        _               -> case nName tgt of
                            ('#':_)         -> return (Just tgt, args)
                            _               -> return (Nothing, args)

                case chan of
                    Just chan' -> invoke chan' rest
                    Nothing    -> say "What channel?"
            }
        | TopicCommand (name:aliases') helpStr invoke <- commands
        ]
    }

------------------------------------------------------------------------
-- Topic action implementations

installTopic :: TopicAction
installTopic chan topic = withTopic chan $ \_ -> do
    lb (send (setTopic chan topic))

reciteTopic :: TopicAction
reciteTopic chan ""       = withTopic chan $ \topic -> do
    say (nName chan ++ ": " ++ topic)
reciteTopic _ ('#':_)     = say "One channel at a time.  Jeepers!"
reciteTopic _ _           = say "I don't know what all that extra stuff is about."

alterTopic :: (String -> String -> String) -> TopicAction
alterTopic f chan args = withTopic chan $ \oldTopic -> do
    lb (send (setTopic chan (f args oldTopic)))

alterListTopic :: (String -> [String] -> [String]) -> TopicAction
alterListTopic f = alterTopic $ \args topic -> show $ case reads topic of
    [(xs, "")]  -> f args xs
    _           -> f args [topic]

------------------------------------------------------------------------

lookupTopic :: Nick -> LB (Maybe String)
lookupTopic chan = gets (\s -> M.lookup (mkCN chan) (ircChannels s))

-- | 'withTopic' is like 'lookupTopic' except that it ditches the Maybe in
--   favor of just yelling at the user when things don't work out as planned.
withTopic :: Nick -> (String -> Cmd Topic ()) -> Cmd Topic ()
withTopic chan f = do
    maybetopic <- lb (lookupTopic chan)
    case maybetopic of
        Just t  -> f t
        Nothing -> say "I don't know that channel."
