module Lambdabot.Compat.PackedNick
    ( PackedNick
    , packNick
    , unpackNick
    ) where

import Lambdabot.Nick
import qualified Data.ByteString.Char8 as BS

-- | The type of nicknames
type PackedNick = BS.ByteString

-- Helper functions
upckStr :: String -> String -> Nick
upckStr def str | null ac   = Nick def str
                | otherwise = Nick bc (tail ac)
    where (bc, ac) = break (==':') str

pckStr :: Nick -> String
pckStr nck = nTag nck ++ ':' : nName nck

-- | Pack a nickname into a ByteString.  Note that the resulting strings are
-- not optimally formatted for human consumtion.
packNick :: Nick -> BS.ByteString
packNick = BS.pack . pckStr

-- | Unpack a nickname packed by 'packNick'.
unpackNick :: BS.ByteString -> Nick
unpackNick = upckStr "freenode" . BS.unpack
