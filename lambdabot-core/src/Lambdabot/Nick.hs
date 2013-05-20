module Lambdabot.Nick
    ( Nick(..)
    , fmtNick
    , parseNick
    ) where

import Lambdabot.Util

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

-- | Format a nickname for display.  This will automatically omit the server
-- field if it is the same as the server of the provided message.
fmtNick :: String -> Nick -> String
fmtNick svr nck
    | nTag nck == svr = nName nck
    | otherwise       = nTag nck ++ ':' : nName nck

-- | Parse a nickname received in a message.  If the server field is not
-- provided, it defaults to the same as that of the message.
parseNick :: String -> String -> Nick
parseNick def str
    | null ac   = Nick def str
    | otherwise = Nick bc ac
    where 
        (bc, ac') = break (==':') str
        ac = drop 1 ac'
