--
-- Provides interface to messages, message pipes
--
module Lambdabot.Message
    ( Message(..)
    , showNick
    , readNick
    , packNick
    , unpackNick
    ) where

import Lambdabot.Nick

import qualified Data.ByteString.Char8 as P

-- TODO: probably remove "Show a" later (used only to implement @echo)
class Show a => Message a where
    -- | extracts the tag of the server involved in a given message
    server      :: a -> String

    -- | extracts the nickname involved in a given message.
    nick        :: a -> Nick

    -- | 'fullName' extracts the full user name involved in a given message.
    fullName    :: a -> String

    -- | 'channels' extracts the channels a Message operate on.
    channels    :: a -> [Nick]

    -- TODO: there must be a better way of handling this ...
    lambdabotName :: a -> Nick

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

-- | Pack a nickname into a ByteString.  Note that the resulting strings are
-- not optimally formatted for human consumtion.
packNick :: Nick -> P.ByteString
packNick = P.pack . pckStr

-- | Unpack a nickname packed by 'packNick'.
unpackNick :: P.ByteString -> Nick
unpackNick = upckStr "freenode" . P.unpack
