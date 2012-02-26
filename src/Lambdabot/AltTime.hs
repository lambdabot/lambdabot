-- | Time compatibility layer
module Lambdabot.AltTime (
    ClockTime,
    getClockTime, diffClockTimes, addToClockTime, timeDiffPretty,
    module System.Time
  ) where

import Control.Arrow (first)

import Data.Binary

import Data.List
import System.Time (TimeDiff(..), noTimeDiff)
import qualified System.Time as T

-- | Wrapping ClockTime (which doesn't provide a Read instance!) seems
-- easier than talking care of the serialization of UserStatus
-- ourselves.
--
newtype ClockTime = ClockTime (T.ClockTime)

instance Eq ClockTime where
    ClockTime (T.TOD x1 y1) == ClockTime (T.TOD x2 y2) =
        x1 == x2 && y1 == y2

instance Show ClockTime where
  showsPrec p (ClockTime (T.TOD x y)) = showsPrec p (x,y)

instance Read ClockTime where
  readsPrec p = map (first $ ClockTime . uncurry T.TOD) . readsPrec p

-- | Retrieve the current clocktime
getClockTime :: IO ClockTime
getClockTime = ClockTime `fmap` T.getClockTime

-- | Difference of two clock times
diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (ClockTime ct1) (ClockTime ct2) =
-- This is an ugly hack (we don't care about picoseconds...) to avoid the
--   "Time.toClockTime: picoseconds out of range"
-- error. I think time arithmetic is broken in GHC.
  (T.diffClockTimes ct1 ct2) { tdPicosec = 0 }

-- | @'addToClockTime' d t@ adds a time difference @d@ and a -- clock
-- time @t@ to yield a new clock time.
addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (ClockTime ct) = ClockTime $ T.addToClockTime td ct

-- | Pretty-print a TimeDiff. Both positive and negative Timediffs produce
--   the same output.
--
-- 14d 17h 8m 53s
--
timeDiffPretty :: TimeDiff -> String
timeDiffPretty td = concat . intersperse " " $ filter (not . null) [
    prettyP years             "y",
    prettyP (months `mod` 12) "m",
    prettyP (days   `mod` 28) "d",
    prettyP (hours  `mod` 24) "h",
    prettyP (mins   `mod` 60) "m",
    prettyP (secs   `mod` 60) "s"]
  where
    prettyP 0 _ = []
    prettyP i s = show i ++ s

    secs = abs $ tdSec td -- This is a hack, but there wasn't an sane output
                          -- for negative TimeDiffs anyway.
    mins   = secs   `div` 60
    hours  = mins   `div` 60
    days   = hours  `div` 24
    months = days   `div` 28
    years  = months `div` 12

------------------------------------------------------------------------

instance Binary ClockTime where
        put (ClockTime (T.TOD i j)) = put i >> put j
        get = do i <- get
                 j <- get
                 return (ClockTime (T.TOD i j))

instance Binary TimeDiff where
        put (TimeDiff ye mo da ho mi se ps) = do
                put ye; put mo; put da; put ho; put mi; put se; put ps
        get = do
                ye <- get
                mo <- get
                da <- get
                ho <- get
                mi <- get
                se <- get
                ps <- get
                return (TimeDiff ye mo da ho mi se ps)


