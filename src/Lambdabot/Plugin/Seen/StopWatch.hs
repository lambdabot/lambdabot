module Lambdabot.Plugin.Seen.StopWatch where

import Lambdabot.Compat.AltTime

import Data.Binary

data StopWatch
    = Stopped !TimeDiff
    | Running !ClockTime
    deriving (Show,Read)

instance Binary StopWatch where
    put (Stopped td) = putWord8 0 >> put td
    put (Running ct) = putWord8 1 >> put ct

    get = getWord8 >>= \h -> case h of
        0 -> fmap Stopped get
        1 -> fmap Running get
        _ -> error "Seen.StopWatch.get"

zeroWatch :: StopWatch
zeroWatch = Stopped noTimeDiff

startWatch :: ClockTime -> StopWatch -> StopWatch
startWatch now (Stopped td) = Running (td `addToClockTime` now)
startWatch _ alreadyStarted = alreadyStarted

stopWatch :: ClockTime -> StopWatch -> StopWatch
stopWatch now (Running t)  = Stopped (t `diffClockTimes` now)
stopWatch _ alreadyStopped = alreadyStopped
