--
-- Provides interface to messages, message pipes
--
module Message where

import Control.Concurrent

-- TODO: probably remove "Show a" later
class Show a => Message a where
    -- | extracts the tag of the server involved in a given message
    server      :: a -> String

    -- | extracts the nickname involved in a given message.
    nick        :: a -> Nick

    -- | 'fullName' extracts the full user name involved in a given message.
    fullName    :: a -> String

    -- | 'names' builds a NAMES message from a list of channels.
    names       :: [Nick] -> a

    -- | 'channels' extracts the channels a Message operate on.
    channels    :: a -> [Nick]

    -- | 'join' creates a join message. String given is the location (channel) to join
    joinChannel :: Nick -> a

    -- | 'part' parts the channel given.
    partChannel :: Nick -> a

    -- | 'getTopic' Returns the topic for a channel, given as a String
    getTopic    :: Nick -> a

    -- | 'setTopic' takes a channel and a topic. It then returns the message
    --   which sets the channels topic.
    setTopic :: Nick -> String -> a

    -- TODO: recheck this. It's usage heavily relies on the fact that message comes from IRC
    body :: a -> [String]

    -- TODO: too IRC-specific
    command :: a -> String

    -- TODO: there must be a better way of handling this ...
    lambdabotName :: a -> Nick

-- |The type of nicknames isolated from a message.
data Nick
  = Nick {
        nTag :: !String, -- ^The tag of the server this nick is on
        nName :: !String -- ^The server-specific nickname of this nick
  } deriving (Eq,Ord)

-- |Format a nickname for display.  This will automatically omit the server
-- field if it is the same as the server of the provided message.
showNick :: Message a => a -> Nick -> String
showNick msg nick_ | nTag nick_ == server msg = nName nick_
                   | otherwise                = nTag nick_ ++ ':' : nName nick_

-- |Parse a nickname received in a message.  If the server field is not
-- provided, it defaults to the same as that of the message.
readNick :: Message a => a -> String -> Nick
readNick msg str | null ac   = Nick (server msg) str
                 | otherwise = Nick bc (tail ac)
    where (bc, ac) = break (==':') str

instance Show Nick where
    show nick_ = show (nTag nick_ ++ ':' : nName nick_)

instance Read Nick where
    readsPrec prec str = map parse' (readsPrec prec str)
        where parse' (str',aft) | null ac   = (Nick "fn" bc, aft)
                                | otherwise = (Nick bc (tail ac), aft)
                  where (bc, ac) = break (==':') str'

type Pipe a = Chan (Maybe a)
