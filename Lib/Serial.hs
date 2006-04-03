-- 
-- Copyright (c) 2004-5 Thomas Jaeger, Don Stewart
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 
--
-- | Serialisation
--
module Lib.Serial (
        Serial(..), 
        stdSerial, mapSerial, listSerial, 
        mapPackedSerial, assocListPackedSerial, mapListPackedSerial,
        readM, Packable(..), {- instances of Packable -}
        packedListSerial,
    ) where

import Data.Maybe               (mapMaybe)

import Data.Map (Map)
import qualified Data.Map as M

import Data.FastPackedString (FastString)
import qualified Data.FastPackedString as P

------------------------------------------------------------------------

-- A flexible (moreso than a typeclass) way to define introduction and
-- elimination for persistent state on a per-module basis.
--
data Serial s = Serial {
        serialize   :: s -> Maybe FastString,
        deserialize :: FastString -> Maybe s
     }

-- | Default `instance' for a Serial
stdSerial :: (Show s, Read s) => Serial s
stdSerial = Serial (Just. P.pack.show) (readM.P.unpack)

-- | Serializes a 'Map' type if both the key and the value are instances
-- of Read and Show. The serialization is done by converting the map to
-- and from lists. Results are saved line-wise, for better editing and
-- revison control.
--
mapSerial :: (Ord k, Show k, Show v, Read k, Read v) => Serial (Map k v)
mapSerial = Serial {
        serialize   = Just . P.pack . unlines . map show . M.toList,
        deserialize = Just . M.fromList . mapMaybe (readM . P.unpack) . P.lines
   }

-- | Serialize a list of 'a's. As for the 'mapSerializer', its output is line-wise.
listSerial :: (Read a, Show a) => Serial [a]
listSerial = Serial {
        serialize   = Just .P.pack . unlines . map show,
        deserialize = Just . mapMaybe (readM . P.unpack) . P.lines
   }

packedListSerial :: Serial [P.FastString]
packedListSerial = Serial {
        serialize   = Just . P.unlines,
        deserialize = Just . P.lines
    }

------------------------------------------------------------------------

-- | 'readM' behaves like read, but catches failure in a monad.
-- this allocates a 20-30 M on startup...
readM :: (Monad m, Read a) => String -> m a
readM s = case [x | (x,t) <- {-# SCC "Serial.readM.reads" #-} reads s    -- bad!
               , ("","")  <- lex t] of
        [x] -> return x
        []  -> fail "Serial.readM: no parse"
        _   -> fail "Serial.readM: ambiguous parse"


class Packable t where
        readPacked :: FastString -> t
        showPacked :: t -> FastString

-- | An instance for Map Packed [Packed]
instance Packable (Map FastString [FastString]) where
        readPacked ps = M.fromList (readKV (P.lines ps))
                where
                readKV :: [FastString] -> [(FastString,[FastString])]
                readKV []       =  []
                readKV (k:rest) = 
                        let (vs, rest') = break (== P.empty) rest
                        in  (k,vs) : readKV (drop 1 rest')


        showPacked m = P.unlines . concatMap (\(k,vs) -> k : vs ++ [P.empty]) $ M.toList m

instance Packable (Map FastString FastString) where
        readPacked ps = M.fromList (readKV (P.lines ps))
                where
                  readKV :: [FastString] -> [(FastString,FastString)]
                  readKV []         = []
                  readKV (k:v:rest) = (k,v) : readKV rest
                  readKV _      = error "Serial.readPacked: parse failed"

        showPacked m  = P.unlines . concatMap (\(k,v) -> [k,v]) $ M.toList m

instance Packable ([(FastString,FastString)]) where
        readPacked ps = readKV (P.lines ps)
                where
                  readKV :: [FastString] -> [(FastString,FastString)]
                  readKV []         = []
                  readKV (k:v:rest) = (k,v) : readKV rest
                  readKV _          = error "Serial.readPacked: parse failed"

        showPacked = P.unlines . concatMap (\(k,v) -> [k,v])

instance Packable (M.Map P.FastString (Bool, [(String, Int)])) where
    readPacked = M.fromList . readKV . P.lines
        where
          readKV :: [P.FastString] -> [(P.FastString,(Bool, [(String, Int)]))]
          readKV []         = []
          readKV (k:v:rest) = (k, (read . P.unpack) v) : readKV rest
          readKV _          = error "Vote.readPacked: parse failed"

    showPacked m = P.unlines . concatMap (\(k,v) -> [k,P.pack . show $ v]) $ M.toList m

-- And for packed string maps
mapPackedSerial :: Serial (Map FastString FastString)
mapPackedSerial = Serial (Just . showPacked) (Just . readPacked)

-- And for list of packed string maps
mapListPackedSerial :: Serial (Map FastString [FastString])
mapListPackedSerial = Serial (Just . showPacked) (Just . readPacked)

-- And for association list
assocListPackedSerial   :: Serial ([(FastString,FastString)])
assocListPackedSerial = Serial (Just . showPacked) (Just . readPacked)

------------------------------------------------------------------------
