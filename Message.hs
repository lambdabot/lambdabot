--
-- Provides interface to messages, message pipes
--
module Message(Message(..), Nick(..), showNick, readNick, Pipe, packNick, unpackNick) where

import qualified Data.ByteString.Char8 as P
import Control.Concurrent (Chan)
import Data.Char (toUpper)

import Control.Arrow (first)

-- TODO: probably remove "Show a" later
class Show a => Message a where
    -- | extracts the tag of the server involved in a given message
    server      :: a -> String

    -- | extracts the nickname involved in a given message.
    nick        :: a -> Nick

    -- | 'fullName' extracts the full user name involved in a given message.
    fullName    :: a -> String

    -- | 'names' builds a NAMES message from a list of channels.
    names       :: String -> [String] -> a

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

-- | The type of nicknames isolated from a message.
data Nick
  = Nick {
        nTag :: !String, -- ^The tag of the server this nick is on
        nName :: !String -- ^The server-specific nickname of this nick
  }

-- This definition of canonicalizeName breaks strict RFC rules, but so does
-- freenode
canonicalizeName :: String -> String
canonicalizeName = map toUpper

instance Eq Nick where
  (Nick tag name) == (Nick tag2 name2) =
     (canonicalizeName name == canonicalizeName name2) && (tag == tag2)

instance Ord Nick where
  (Nick tag name) <= (Nick tag2 name2) =
     (tag, canonicalizeName name) <= (tag2, canonicalizeName name2)


-- Helper functions
upckStr :: String -> String -> Nick
upckStr def str | null ac   = Nick def str
                | otherwise = Nick bc (tail ac)
    where (bc, ac) = break (==':') str

pckStr :: Nick -> String
pckStr nck = nTag nck ++ ':' : nName nck

-- | Format a nickname for display.  This will automatically omit the server
-- field if it is the same as the server of the provided message.
showNick :: Message a => a -> Nick -> String
showNick msg nick_ | nTag nick_ == server msg = nName nick_
                   | otherwise                = pckStr nick_

-- | Parse a nickname received in a message.  If the server field is not
-- provided, it defaults to the same as that of the message.
readNick :: Message a => a -> String -> Nick
readNick msg str = upckStr (server msg) str'
        where str' | last str `elem` ":" = init str
                   | otherwise           = str

instance Show Nick where
    show x | nTag x == "freenode" = show $ nName x
           | otherwise            = show $ pckStr x

instance Read Nick where
    readsPrec prec str = map (first (upckStr "freenode")) (readsPrec prec str)

-- | Pack a nickname into a ByteString.  Note that the resulting strings are
-- not optimally formatted for human consumtion.
packNick :: Nick -> P.ByteString
packNick = P.pack . pckStr

-- | Unpack a nickname packed by 'packNick'.
unpackNick :: P.ByteString -> Nick
unpackNick = upckStr "freenode" . P.unpack

type Pipe a = Chan (Maybe a)
