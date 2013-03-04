module Lambdabot.Nick
    ( Nick(..)
    ) where

import Lambdabot.Util

import Control.Arrow
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

-- TODO: don't hardcode this
instance Show Nick where
    show x | nTag x == "freenode" = show $ nName x
           | otherwise            = show $ pckStr x

instance Read Nick where
    readsPrec prec str = map (first (upckStr "freenode")) (readsPrec prec str)

-- Helper functions
upckStr :: String -> String -> Nick
upckStr def str | null ac   = Nick def str
                | otherwise = Nick bc (tail ac)
    where (bc, ac) = break (==':') str

pckStr :: Nick -> String
pckStr nck = nTag nck ++ ':' : nName nck

