module Lambdabot.Nick
    ( Nick(..)
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
canonicalizeName :: String -> String
canonicalizeName = dropSpace . map toUpper

instance Eq Nick where
  (Nick tag name) == (Nick tag2 name2) =
     (canonicalizeName name == canonicalizeName name2) && (tag == tag2)

instance Ord Nick where
  (Nick tag name) <= (Nick tag2 name2) =
     (tag, canonicalizeName name) <= (tag2, canonicalizeName name2)
