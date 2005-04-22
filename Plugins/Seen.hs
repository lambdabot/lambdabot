--
-- | Keep track of IRC users.
--
module Plugins.Seen (theModule) where

import IRC
import Util (mapSerializer, lowerCaseString, firstWord, listToStr, debugStrLn)
import Config
import qualified Map as M

import Data.List           ((\\), nub)

import Control.Monad       (unless)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Arrow       (first)

import System.Time (TimeDiff(..), noTimeDiff)
import qualified System.Time as T 
  (ClockTime(..), getClockTime, diffClockTimes, addToClockTime)

------------------------------------------------------------------------

newtype SeenModule = SeenModule ()

theModule :: MODULE
theModule = MODULE $ SeenModule ()

-- | The type of channels
type Channel = String
-- | The type of nicknames
type Nick = String

-- | We last heard the user speak at ClockTime; since then we have missed
--   TimeDiff of him because we were absent.
type LastSpoke = Maybe (ClockTime, TimeDiff)

-- | 'UserStatus' keeps track of the status of a given Nick name.
data UserStatus
        = Present LastSpoke [Channel]  
          -- ^ Records when the nick last spoke and that the nick is currently 
          --   in [Channel].
        | NotPresent ClockTime StopWatch Channel
          -- ^ The nick is not present and was last seen at ClockTime in Channel.
          --   The second argument records how much we've missed.
        | WasPresent ClockTime StopWatch LastSpoke Channel
          -- ^ The bot parted a channel where the user was. The Clocktime
          --   records the time and Channel the channel this happened in.
          --   We also save the reliablility of our information and the
          --   time we last heard the user speak.
        | NewNick Nick                 
          -- ^ The user changed nick to something new.
    deriving (Show, Read)

type SeenState = M.Map Nick UserStatus
type Seen m a = ModuleT SeenState m a

instance Module SeenModule SeenState where
    moduleHelp _ _      = return "Report if a user has been seen by the bot"
    moduleCmds _        = return ["seen"]
    moduleDefState _    = return M.empty
    moduleSerialize _   = Just mapSerializer
    moduleInit _
      = do ircSignalConnect "JOIN"    joinCB
           ircSignalConnect "PART"    partCB
           ircSignalConnect "QUIT"    quitCB
           ircSignalConnect "NICK"    nickCB
           ircSignalConnect "353"     joinChanCB
           ircSignalConnect "PRIVMSG" msgCB

    -- This magically causes the 353 callback to be invoked :)
    moduleDynInit _     = ircNames =<< ircGetChannels
    
    moduleExit _ = do
        chans <- ircGetChannels
        unless (null chans) $ do
            ct    <- liftIO getClockTime
            fm    <- readMS
            writeMS $ botPart ct chans fm

    process m msg target cmd rest =
      do seenFM <- readMS
         now <- liftIO getClockTime
         let myname = lowerCaseString (name config)
             nick' = firstWord rest
             you   = nick' == ircNick msg
             nick  = if you then "you" else nick'
             lcnick = lowerCaseString nick'
             ircMessage = ircPrivmsg target . concat
             clockDifference = timeDiffPretty . diffClockTimes now
             -- I guess the only way out of this spagetty hell are 
             -- printf-style responses.
             nickPresent mct cs = ircMessage [
                 if you then "You are" else nick ++ " is", " in ",
                 listToStr "and" cs, ".",
                 case mct of
                   Nothing -> 
                     concat [" I don't know when ", nick, " last spoke."]
                   Just (ct,missed)
                     | all (==' ') missedPretty
                     -> concat [" Last spoke ", lastSpoke, "."]
                     |  otherwise
                     -> concat [" I last heard ", nick, " speak ", lastSpoke,
                                " but I have missed ", 
                                missedPretty, " since then."]
                     where 
                       lastSpoke | all (==' ') when' = "just now"
                                 | otherwise         = when' ++ " ago"
                       when' = clockDifference ct
                       missedPretty = timeDiffPretty missed
               ]
             nickNotPresent ct missed chan = ircMessage [
                 "I saw ", nick, " leaving ", chan, " ", 
                 clockDifference ct, " ago", 
                 case missed of
                   Stopped missed' 
                     |  missedPretty <- timeDiffPretty missed', 
                        any (/=' ') missedPretty
                     -> concat [" but I have missed ", missedPretty, 
                                " since then."]
                   _ -> "."
               ]
             nickWasPresent ct chan =
               ircMessage ["Last time I saw ", nick, "was when I left ",
                           chan , " ", clockDifference ct, " ago."]
             nickIsNew newnick =
               do let findFunc str =
                        case M.lookup (lowerCaseString str) seenFM of
                          Just (NewNick str') -> findFunc str'
                          Just _              -> str
                          Nothing             -> error "SeenModule.nickIsNew: Nothing"
                      us = findFunc newnick
                  ircMessage [if you then "You have" else nick++" has", 
                              " changed nick to ", us, "."]
                  process m msg target cmd us
         if lcnick == myname
            then ircMessage ["Yes, I'm here."]
            else case M.lookup lcnick seenFM of
                  Just (Present mct cs) -> nickPresent mct cs
                  Just (NotPresent ct td chan) -> nickNotPresent ct td chan
                  Just (WasPresent ct _ _todo chan) -> nickWasPresent ct chan
                  Just (NewNick newnick) -> nickIsNew newnick
                  _ -> ircMessage ["I haven't seen ", nick, "."]

-- | Callback for when somebody joins. If it is not the bot that joins, record
--   that we have a new user in our state tree and that we have never seen the
--   user speaking.
joinCB :: IRCMessage -> Seen IRC () -- when somebody joins
joinCB msg = withSeenFM msg $ \fm _ct myname nick ->
  if nick /= myname
     then let newInfo = Present Nothing (ircChans msg)
          in  Right $ M.insertUpd (updateJ Nothing (ircChans msg)) nick newInfo fm
     else Right fm

botPart :: ClockTime -> [Channel] -> SeenState -> SeenState
botPart ct chans fm = 
    let botPart' cs us =
          case us of
            Present mct xs ->
              case xs \\ cs of
                [] -> WasPresent ct (startWatch ct zeroWatch) mct (head cs)
                ys -> Present mct ys
            NotPresent ct' missed c 
              | c `elem` chans -> NotPresent ct' (startWatch ct missed) c
            _ -> us
    in fmap (botPart' chans) fm

partCB :: IRCMessage -> Seen IRC () -- when somebody parts
partCB msg = withSeenFM msg $ \fm ct myname nick ->
  if nick == myname
       then Right $ botPart ct (ircChans msg) fm
       else case M.lookup nick fm of
              Just (Present mct xs) ->
                case xs \\ ircChans msg of
                  [] -> Right $ M.insert nick
                         (NotPresent ct zeroWatch (head xs))
                         fm
                  ys -> Right $ M.insert nick
                                         (Present mct ys)
                                         fm
              _ -> Left "SeenModule> someone who isn't known parted"

quitCB :: IRCMessage -> Seen IRC () -- when somebody quits
quitCB msg = withSeenFM msg $ \fm ct _myname nick ->
  case M.lookup nick fm of
    Just (Present _ct xs) -> Right $ M.insert nick 
        (NotPresent ct zeroWatch (head xs)) fm
    _ -> Left "SeenModule> someone who isn't known has quit"

nickCB :: IRCMessage -> Seen IRC () -- when somebody changes his/her name
nickCB msg = withSeenFM msg $ \fm _ct _myname nick ->
  let newnick = drop 1 $ head (msgParams msg)
      lcnewnick = lowerCaseString newnick
  in case M.lookup nick fm of
       Just status ->
         let fm' = M.insert nick (NewNick newnick) fm
         in Right $ M.insert lcnewnick status fm'
       _ -> Left "SeenModule> someone who isn't here changed nick"

-- use IRC.ircChans?
joinChanCB :: IRCMessage -> Seen IRC () -- when the bot join a channel
joinChanCB msg = withSeenFM msg $ \fm now _myname _nick ->
  let l = msgParams msg
      chan = l !! 2
      chanUsers = words (drop 1 (l !! 3)) -- remove ':'
      insertNick fm' u = M.insertUpd (updateJ (Just now) [chan])
                                      (lowerCaseString $ unUserMode u)
                                      (Present Nothing [chan])
                                      fm'
      in Right $ fmap (updateNP now chan) $ foldl insertNick fm chanUsers

-- when somebody speaks, update their clocktime
msgCB :: IRCMessage -> Seen IRC ()
msgCB msg = withSeenFM msg $ \fm ct _myname nick ->
  case M.lookup nick fm of
    Just (Present _ xs) -> Right $ 
      M.insert nick (Present (Just (ct, noTimeDiff)) xs) fm
    _ -> Left "SeenModule> someone who isn't here msg us"

-- misc. functions
unUserMode :: Nick -> Nick
unUserMode nick = dropWhile (`elem` "@+") nick

withSeenFM :: IRCMessage
              -> (SeenState -> ClockTime -> String -> Nick
                  -> Either String SeenState)
              -> Seen IRC ()
withSeenFM msg f = do 
    let nick = lowerCaseString . unUserMode . ircNick $ msg
    state <- readMS
    ct <- liftIO getClockTime
    let myname = (lowerCaseString . name) config
    case f state ct myname nick of
        Right newstate -> writeMS newstate
        Left err -> debugStrLn err

-- | Update the user status.
updateJ :: Maybe ClockTime -- ^ If the bot joined the channel, the time that 
                           --   happened, i.e. now.
  -> [Channel]             -- ^ The channels the user joined.
  -> UserStatus            -- ^ The old status
  -> UserStatus            -- ^ The new status
-- The user was present before, so he's present now.
updateJ _ c (Present ct cs) = Present ct $ nub (c ++ cs)
-- The user was present when we left that channel and now we've come back.
-- We need to update the time we've missed.
updateJ (Just now) cs (WasPresent lastSeen _ (Just (lastSpoke, missed)) channel)
  | channel `elem` cs
  ---                 newMissed
  --- |---------------------------------------|
  --- |-------------------|                   |
  ---        missed    lastSeen              now
  = let newMissed = addToClockTime missed now `diffClockTimes` lastSeen
    in  newMissed `seq` Present (Just (lastSpoke, newMissed)) cs
-- Otherwise, we create a new record of the user.
updateJ _ cs _ = Present Nothing cs

-- | Update a user who is not present. We just convert absolute missing time
--   into relative time.
updateNP :: ClockTime -> Channel -> UserStatus -> UserStatus
updateNP now _ (NotPresent ct missed c)
  = NotPresent ct (stopWatch now missed) c
-- The user is gone, thus it's meaningless when we last heard him speak.
updateNP now chan (WasPresent lastSeen missed (Just _) c)
  | c == chan = WasPresent lastSeen (stopWatch now missed) Nothing c
updateNP _ _ status = status
-- annoying
timeDiffPretty :: TimeDiff -> String
timeDiffPretty td =
  let secs = abs $ tdSec td -- This is a hack, but there wasn't an sane output
                            -- for negative TimeDiffs anyway.
      mins = secs `div` 60
      hours = mins `div` 60
      days = hours `div` 24
      months = days `div` 28
      years = months `div` 12
  in listToStr "and" $ filter (not . null) [
             prettyP years "year",
             prettyP (months `mod` 12) "month",
             prettyP (days `mod` 28) "day",
             prettyP (hours `mod` 24) "hour",
             prettyP (mins `mod` 60) "minute",
             prettyP (secs `mod` 60) "second"]
              where prettyP i str
                     | i == 0    = ""
                     | i == 1    = "1 " ++ str
                     | otherwise = show i ++ " " ++ str ++ "s"

------------------------------------------------------------------------

------- Time compatibility layer (maybe move to its own module?) -------

-- Wrapping ClockTime (which doesn't provide a Read instance!) seems 
-- easier than talking care of the serialization of UserStatus ourselves.
newtype ClockTime = ClockTime (T.ClockTime)

instance Show ClockTime where
  showsPrec p (ClockTime (T.TOD x y)) = showsPrec p (x,y)

instance Read ClockTime where
  readsPrec p = map (first $ ClockTime . uncurry T.TOD) . readsPrec p

getClockTime :: IO ClockTime
getClockTime = ClockTime `fmap` T.getClockTime

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (ClockTime ct1) (ClockTime ct2) = 
-- This is an ugly hack (we don't care about picoseconds...) to avoid the 
--   "Time.toClockTime: picoseconds out of range"
-- error. I think time arithmetic is broken in GHC.
  (T.diffClockTimes ct1 ct2) { tdPicosec = 0 }

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (ClockTime ct) = ClockTime $ T.addToClockTime td ct

------------------------------------------------------------------------

-- Stop watches mini-library --
data StopWatch = Stopped TimeDiff | Running ClockTime deriving (Show,Read)

zeroWatch :: StopWatch
zeroWatch = Stopped noTimeDiff

startWatch :: ClockTime -> StopWatch -> StopWatch
startWatch now (Stopped td) = Running $ td `addToClockTime` now
startWatch _ alreadyStarted = alreadyStarted

stopWatch :: ClockTime -> StopWatch -> StopWatch
stopWatch now (Running t)  = Stopped $ t `diffClockTimes` now
stopWatch _ alreadyStopped = alreadyStopped

