-- | Time compatibility layer
-- (stuff to support old lambdabot state serialization formats)
--
-- TODO: trim this down to just the explicitly serialization-related stuff
module Lambdabot.Compat.AltTime 
    ( ClockTime
    , getClockTime
    , diffClockTimes
    , addToClockTime
    , timeDiffPretty
    
    , TimeDiff(..)
    , noTimeDiff
    ) where

import Control.Arrow (first)

import Data.Binary

import Data.List
import Data.Time
import Text.Read hiding (get, lexP, readPrec)
import Text.Read.Lex

-- | Wrapping ClockTime (which doesn't provide a Read instance!) seems
-- easier than talking care of the serialization of UserStatus
-- ourselves.
--
newtype ClockTime = ClockTime UTCTime
    deriving Eq

newtype TimeDiff = TimeDiff NominalDiffTime
    deriving (Eq, Ord)

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0

-- convert to/from the format in old-time, so we can serialize things 
-- in the same way as older versions of lambdabot.
toOldTime :: ClockTime -> (Integer, Integer)
toOldTime (ClockTime t) = round (diffUTCTime t epoch * 1e12) `divMod` 1000000000000

fromOldTime :: Integer -> Integer -> ClockTime
fromOldTime x y = ClockTime (addUTCTime (fromIntegral x + fromIntegral y * 1e-12) epoch)

instance Show ClockTime where
    showsPrec p = showsPrec p . toOldTime

instance Read ClockTime where
    readsPrec p = map (first (uncurry fromOldTime)) . readsPrec p

instance Show TimeDiff where
    showsPrec p td = showParen (p > 10) $
        ( showString "TimeDiff {tdYear = "
        . showsPrec 11 ye
        . showString ", tdMonth = "
        . showsPrec 11 mo
        . showString ", tdDay = "
        . showsPrec 11 da
        . showString ", tdHour = "
        . showsPrec 11 ho
        . showString ", tdMin = "
        . showsPrec 11 mi
        . showString ", tdSec = "
        . showsPrec 11 se
        . showString ", tdPicosec = "
        . showsPrec 11 ps
        . showString "}")
        where (ye, mo, da, ho, mi, se, ps) = toOldTimeDiff td

instance Read TimeDiff where
    readsPrec = readPrec_to_S $ parens
        (prec 11 (do
            let lexP = lift Text.Read.Lex.lex
                readPrec :: Read a => ReadPrec a
                readPrec = readS_to_Prec readsPrec
            Ident "TimeDiff"    <- lexP
            Punc "{"            <- lexP
            Ident "tdYear"      <- lexP
            Punc "="            <- lexP
            ye                  <- reset readPrec
            Punc ","            <- lexP
            Ident "tdMonth"     <- lexP
            Punc "="            <- lexP
            mo                  <- reset readPrec
            Punc ","            <- lexP
            Ident "tdDay"       <- lexP
            Punc "="            <- lexP
            da                  <- reset readPrec
            Punc ","            <- lexP
            Ident "tdHour"      <- lexP
            Punc "="            <- lexP
            ho                  <- reset readPrec
            Punc ","            <- lexP
            Ident "tdMin"       <- lexP
            Punc "="            <- lexP
            mi                  <- reset readPrec
            Punc ","            <- lexP
            Ident "tdSec"       <- lexP
            Punc "="            <- lexP
            se                  <- reset readPrec
            Punc ","            <- lexP
            Ident "tdPicosec"   <- lexP
            Punc "="            <- lexP
            ps                  <- reset readPrec
            Punc "}"            <- lexP
            return (fromOldTimeDiff ye mo da ho mi se ps)))
    readList = readListDefault
    readListPrec = readListPrecDefault

-- | Retrieve the current clocktime
getClockTime :: IO ClockTime
getClockTime = ClockTime `fmap` getCurrentTime

-- | Difference of two clock times
diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (ClockTime ct1) (ClockTime ct2) = TimeDiff (diffUTCTime ct1 ct2)

-- | @'addToClockTime' d t@ adds a time difference @d@ and a -- clock
-- time @t@ to yield a new clock time.
addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime (TimeDiff td) (ClockTime ct) = ClockTime (addUTCTime td ct)

-- | Pretty-print a TimeDiff. Both positive and negative Timediffs produce
--   the same output.
--
-- 14d 17h 8m 53s
--
timeDiffPretty :: TimeDiff -> String
timeDiffPretty td = concat . intersperse " " $ filter (not . null)
    [ prettyP ye "y"
    , prettyP mo "m"
    , prettyP da "d"
    , prettyP ho "h"
    , prettyP mi "m"
    , prettyP se "s"
    ]
  where
    prettyP 0 _ = []
    prettyP i s = show i ++ s
    
    (ye, mo, da, ho, mi, se, _) = toOldTimeDiff td

toOldTimeDiff :: TimeDiff -> (Int, Int, Int, Int, Int, Int, Integer)
toOldTimeDiff (TimeDiff td) = (fromInteger ye, fromInteger mo, fromInteger da, fromInteger ho, fromInteger mi, fromInteger se, ps)
    where
        (a,  ps) = round (td * 1e12) `divMod` 1000000000000
        (b,  se) = a `divMod` 60
        (c,  mi) = b `divMod` 60
        (d,  ho) = c `divMod` 24
        (e,  da) = d `divMod` 28
        (ye, mo) = e `divMod` 12

fromOldTimeDiff :: Int -> Int -> Int -> Int -> Int -> Int -> Integer -> TimeDiff
fromOldTimeDiff ye mo da ho mi se ps =
    TimeDiff
        (1e-12 * fromIntegral (ps 
            + 1000000000000 * (toInteger se 
                + 60 * (toInteger mi 
                    + 60 * (toInteger ho
                        + 24 * (toInteger da
                            + 28 * (toInteger mo
                                + 12 * toInteger ye)))))))

------------------------------------------------------------------------

instance Binary ClockTime where
        put t = put i >> put j
            where (i, j) = toOldTime t
        get = do 
            i <- get
            j <- get
            return (fromOldTime i j)

instance Binary TimeDiff where
        put td = do
            put ye; put mo; put da; put ho; put mi; put se; put ps
            where (ye, mo, da, ho, mi, se, ps) = toOldTimeDiff td
        get = do
            ye <- get
            mo <- get
            da <- get
            ho <- get
            mi <- get
            se <- get
            ps <- get
            return (fromOldTimeDiff ye mo da ho mi se ps)


