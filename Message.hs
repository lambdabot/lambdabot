--
-- Provides interface to messages, message pipes
--
module Message where

import Control.Concurrent

-- TODO: probably remove "Show a" later
class Show a => Message a where
  nick :: a -> String -- | extracts the nickname involved in a given message.
  fullName :: a -> String -- | 'fullName' extracts the full user name involved in a given message.
  names :: [String] -> a -- | 'names' builds a NAMES message from a list of channels.
  channels :: a -> [String] -- | 'channels' extracts the channels a Message operate on.
  joinChannel :: String -> a -- | 'join' creates a join message. String given is the location (channel) to join
  partChannel :: String -> a -- | 'part' parts the channel given.
  getTopic :: String -> a -- | 'getTopic' Returns the topic for a channel, given as a String
  setTopic :: String -> String -> a -- | 'setTopic' takes a channel and a topic. It then returns the message
                                    --   which sets the channels topic.
  body :: a -> [String] -- TODO: recheck this. It's usage heavily relies on the fact that message comes from IRC

type Pipe a = Chan (Maybe a)
