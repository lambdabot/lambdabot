module TopicModule (TopicModule, topicModule, theModule) where
--     $Id: TopicModule.hs,v 1.8 2003/07/29 13:03:02 eris Exp $

import IRC
import qualified Map as M

import Control.Monad.State (gets)
import Control.Exception (catchJust, errorCalls)
import Char (isSpace)

newtype TopicModule = TopicModule ()

theModule = MODULE topicModule
topicModule = TopicModule ()

instance Module TopicModule where
  moduleName   _ = return "topic"
  moduleSticky _ = False
  commands     _ = return ["topic-tell",
                           "topic-cons", "topic-snoc",
                           "topic-tail", "topic-init"]
  process _ _ src "topic-cons" text = topic_cons src text
  process _ _ src "topic-snoc" text = topic_snoc src text
  process _ _ src "topic-tail" chan = alter_topic src chan tail
  process _ _ src "topic-init" chan = alter_topic src chan init
  process _ _ src "topic-tell" chan =
    do
    maybetopic <- gets (\s -> M.lookup (mkCN chan) (ircChannels s) )
    case maybetopic of
                    Just x  -> ircPrivmsg src x
                    Nothing -> ircPrivmsg src "don't know that channel"
  process _ _ src cmd _
    = ircPrivmsg src ("Bug! someone forgot the handler for \""++cmd++"\"")

topic_snoc :: String -> String -> IRC ()
topic_snoc source cmdtext = alter_topic source chan (snoc topic_item)
  where
  (chan, topic_item) = split_first_word cmdtext

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

topic_cons :: String -> String -> IRC ()
topic_cons source cmdtext = alter_topic source chan (topic_item:)
  where
  (chan, topic_item) = split_first_word cmdtext

split_first_word :: String -> (String, String)
split_first_word xs = (w, dropWhile isSpace xs')
  where (w, xs') = break isSpace xs
  
alter_topic :: String -> String -> ([String] -> [String]) -> IRC ()
alter_topic source chan f
  = do
    maybetopic <- gets (\s -> M.lookup (mkCN chan) (ircChannels s) )
    case maybetopic
         of
         Just x -> case reads x
                        of
                        [(xs,"")] -> ircTopic chan (show $ f $ xs)
                        [(xs,r)] | length r <= 2
                                  -> -- probably bogus characters near end,
                                     -- do anyway
                                     do  ircPrivmsg source $ 
                                           "ignoring bogus characters: " ++ r
                                         ircTopic chan (show $ f $ xs)
                        _         -> ircPrivmsg source "topic doesn't parse"
         Nothing -> ircPrivmsg chan "don't know that channel"
