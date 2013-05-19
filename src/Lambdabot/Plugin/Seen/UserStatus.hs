module Lambdabot.Plugin.Seen.UserStatus where

import Control.Applicative
import Data.Binary
import qualified Data.ByteString as BS
import Data.List
import Lambdabot.Compat.AltTime
import Lambdabot.Compat.PackedNick
import Lambdabot.Plugin.Seen.StopWatch

-- | The type of channels
type Channel = BS.ByteString

-- | We last heard the user speak at ClockTime; since then we have missed
--   TimeDiff of him because we were absent.
type LastSpoke = Maybe (ClockTime, TimeDiff)

-- | 'UserStatus' keeps track of the status of a given Nick name.
data UserStatus
    = Present    !LastSpoke [Channel]
        -- ^ Records when the nick last spoke and that the nick is currently
        --   in [Channel].
    | NotPresent !ClockTime !StopWatch [Channel]
        -- ^ The nick is not present and was last seen at ClockTime in Channel.
        --   The second argument records how much we've missed.
    | WasPresent !ClockTime !StopWatch !LastSpoke [Channel]
        -- ^ The bot parted a channel where the user was. The Clocktime
        --   records the time and Channel the channel this happened in.
        --   We also save the reliablility of our information and the
        --   time we last heard the user speak.
    | NewNick !PackedNick
        -- ^ The user changed nick to something new.
    deriving (Show, Read)

instance Binary UserStatus where
    put (Present sp ch)          = putWord8 0 >> put sp >> put ch
    put (NotPresent ct sw ch)    = putWord8 1 >> put ct >> put sw >> put ch
    put (WasPresent ct sw sp ch) = putWord8 2 >> put ct >> put sw >> put sp >> put ch
    put (NewNick n)              = putWord8 3 >> put n

    get = getWord8 >>= \h -> case h of
        0 -> Present    <$> get <*> get
        1 -> NotPresent <$> get <*> get <*> get
        2 -> WasPresent <$> get <*> get <*> get <*> get
        3 -> NewNick    <$> get
        _ -> error "Seen.UserStatus.get"

-- | Update the user status when a user joins a channel.
updateJ :: Maybe ClockTime -- ^ If the bot joined the channel, the time that
                           --   happened, i.e. now.
    -> [Channel]           -- ^ The channels the user joined.
    -> UserStatus          -- ^ The old status
    -> UserStatus          -- ^ The new status
-- The user was present before, so he's present now.
updateJ _ c (Present ct cs) = Present ct $ nub (c ++ cs)
-- The user was present when we left that channel and now we've come back.
-- We need to update the time we've missed.
updateJ (Just now) cs (WasPresent lastSeen _ (Just (lastSpoke, missed)) channels)
    | head channels `elem` cs
    ---                 newMissed
    --- |---------------------------------------|
    --- |-------------------|                   |
    ---        missed    lastSeen              now
    = let newMissed = addToClockTime missed now `diffClockTimes` lastSeen
       in newMissed `seq` Present (Just (lastSpoke, newMissed)) cs
-- Otherwise, we create a new record of the user.
updateJ _ cs _ = Present Nothing cs

-- | Update a user who is not present. We just convert absolute missing time
--   into relative time (i.e. start the "watch").
updateNP :: ClockTime -> Channel -> UserStatus -> UserStatus
updateNP now _ (NotPresent ct missed c)
    = NotPresent ct (stopWatch now missed) c
-- The user might be gone, thus it's meaningless when we last heard him speak.
updateNP now chan (WasPresent lastSeen missed _ cs)
    | head cs == chan = WasPresent lastSeen (stopWatch now missed) Nothing cs
updateNP _ _ status = status
