-- | Backward-compatibility shim for (de-)serializing 'Nick's
-- using the old 'Read'/'Show' instances which gave freenode
-- special treatment.
module Lambdabot.Compat.FreenodeNick
    ( FreenodeNick(..)
    , freenodeNickMapSerial
    ) where

import Control.Arrow
import qualified Data.Map as M
import Lambdabot.Nick
import Lambdabot.Util.Serial

newtype FreenodeNick = FreenodeNick { getFreenodeNick :: Nick }
    deriving (Eq, Ord)

instance Show FreenodeNick where
    show (FreenodeNick x)
        | nTag x == "freenode" = show $ nName x
        | otherwise            = show $ pckStr x

instance Read FreenodeNick where
    readsPrec prec str = map (first (FreenodeNick . upckStr "freenode")) (readsPrec prec str)

-- Helper functions
upckStr :: String -> String -> Nick
upckStr def str
    | null ac   = Nick def str
    | otherwise = Nick bc (tail ac)
    where (bc, ac) = break (==':') str

pckStr :: Nick -> String
pckStr nck = nTag nck ++ ':' : nName nck

freenodeNickMapSerial :: (Show v, Read v) => Serial (M.Map Nick v)
freenodeNickMapSerial = Serial
    (serialize mapSerial . M.mapKeysMonotonic FreenodeNick)
    (fmap (M.mapKeysMonotonic getFreenodeNick) . deserialize mapSerial)
