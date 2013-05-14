module Lambdabot.Nick
    ( Nick(..)
    , showNick'
    , readNick'
    , packNick
    , unpackNick
    ) where

import Lambdabot.Util

import qualified Data.ByteString.Char8 as P
import Data.Char

-- | The type of nicknames isolated from a message.
data Nick = Nick 
    { nTag  :: !String -- ^ The tag of the server this nick is on
    , nName :: !String -- ^ The server-specific nickname of this nick
    }

-- This definition of canonicalizeName breaks strict RFC rules, but so does
-- freenode
-- TODO: server-specific rules should have server-specific implementations
canonicalizeName :: String -> String
canonicalizeName = dropSpace . map toUpper

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
showNick' :: String -> Nick -> String
showNick' svr nick_ | nTag nick_ == svr = nName nick_
                   | otherwise         = pckStr nick_

-- | Parse a nickname received in a message.  If the server field is not
-- provided, it defaults to the same as that of the message.
readNick' :: String -> String -> Nick
readNick' svr str = upckStr svr str'
        where str' | last str `elem` ":" = init str
                   | otherwise           = str

-- | Pack a nickname into a ByteString.  Note that the resulting strings are
-- not optimally formatted for human consumtion.
packNick :: Nick -> P.ByteString
packNick = P.pack . pckStr

-- | Unpack a nickname packed by 'packNick'.
unpackNick :: P.ByteString -> Nick
unpackNick = upckStr "freenode" . P.unpack
