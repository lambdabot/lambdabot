--
-- Provides interface to messages, message pipes
--
module Message where

import Control.Concurrent

-- TODO: probably remove "Show a" later
class Show a => Message a where
    -- | extracts the nickname involved in a given message.
    nick        :: a -> String

    -- | 'fullName' extracts the full user name involved in a given message.
    fullName    :: a -> String

    -- | 'names' builds a NAMES message from a list of channels.
    names       :: [String] -> a

    -- | 'channels' extracts the channels a Message operate on.
    channels    :: a -> [String]

    -- | 'join' creates a join message. String given is the location (channel) to join
    joinChannel :: String -> a

    -- | 'part' parts the channel given.
    partChannel :: String -> a

    -- | 'getTopic' Returns the topic for a channel, given as a String
    getTopic    :: String -> a

    -- | 'setTopic' takes a channel and a topic. It then returns the message
    --   which sets the channels topic.
    setTopic :: String -> String -> a

    -- TODO: recheck this. It's usage heavily relies on the fact that message comes from IRC
    body :: a -> [String]

    -- TODO: too IRC-specific
    command :: a -> String

type Pipe a = Chan (Maybe a)
