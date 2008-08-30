{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- Copyright (c) 2004 Thomas Jaeger
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Keep track of IRC users.
module Plugin.Seen (theModule) where

import Data.Binary

import File (findFile)
import Plugin
import Lambdabot.AltTime
import Lambdabot.Error         (tryError)
import Lambdabot.Util          (lowerCaseString)

import qualified Message as G (Message, names, channels, nick, packNick, unpackNick, Nick(..), body, lambdabotName, showNick, readNick)

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy as L

import System.Directory

import System.Time (normalizeTimeDiff) -- or export from AltTime.hs?

import Control.Monad       (unless, zipWithM_)
import Control.Arrow       (first)
import Text.Printf

$(plugin "Seen")

-- Try using packed strings?

-- | The type of channels
type Channel = P.ByteString

-- | The type of nicknames
type Nick = P.ByteString

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
        | NewNick !Nick
          -- ^ The user changed nick to something new.
    deriving (Show, Read)

data StopWatch = Stopped !TimeDiff
               | Running !ClockTime
        deriving (Show,Read)

type SeenState = (MaxMap, SeenMap)
type SeenMap   = M.Map Nick UserStatus
type MaxMap    = M.Map String Int
type Seen m a  = ModuleT SeenState m a

------------------------------------------------------------------------

-- ok, since this module generates quite a lot of state, what we'll do
-- is use Binary to pack this value, since Read is sooo slow and exe (as
-- my gf says :)

{-
instance Binary (M.Map Nick UserStatus) where
    put_ bh m = put_ bh (M.toList m)
    get bh    = do x <- get bh ; return (M.fromList x)
-}

instance Binary StopWatch where
    put (Stopped td) = putWord8 0 >> put td
    put (Running ct) = putWord8 1 >> put ct

    get = do
        h <- getWord8
        case h of
            0 -> liftM Stopped get
            1 -> liftM Running get
            _ -> error "Seen.StopWatch.get"

instance Binary UserStatus where
    put (Present spoke chans) = do
        putWord8 0
        put spoke
        put chans
    put (NotPresent ct sw chans) = do
        putWord8 1
        put ct
        put sw
        put chans
    put (WasPresent ct sw spoke chans) = do
        putWord8 2
        put ct
        put sw
        put spoke
        put chans
    put (NewNick n) = putWord8 3 >> put n

    get = do
        h <- getWord8
        case h of
            0 -> do
                x <- get
                y <- get
                return (Present x y)
            1 -> do
                x <- get
                y <- get
                z <- get
                return (NotPresent x y z)
            2 -> do
                x <- get
                y <- get
                z <- get
                a <- get
                return (WasPresent x y z a)
            3 -> do
                x <- get
                return (NewNick x)

            _ -> error "Seen.UserStatus.get"

------------------------------------------------------------------------
--
-- something's broken. doesn't seem to correctly keep the seen data over
-- reboots anymore :/
--

instance Module SeenModule SeenState where
    moduleHelp _ "seen"  = "seen <user>. Report if a user has been seen by the bot"
    moduleHelp _ "users" = "users [chan]. Report the maximum number of users seen in a channel, and active users in the last 30 minutes"
    moduleCmds _         = ["users","seen"]
    moduleDefState _     = return (M.empty,M.empty)

    -- first step towards tracking the maximum number of users
    process _ msg chan "users" rest = do
         (m, seenFM) <- readMS
         s <- io getClockTime
         let who = G.packNick $ lcNick $ if null rest then chan else G.readNick msg rest
             now = length [ () | (_,Present _ chans) <- M.toList seenFM
                               , who `elem` chans ]

             n = case M.lookup (P.unpack who) m of Nothing -> 1; Just n' -> n'

             active = length [() | (_,st@(Present _ chans)) <- M.toList seenFM
                                 , who `elem` chans && isActive st ]

             isActive (Present (Just (ct,_td)) _cs) = recent ct
             isActive _                             = False

             recent t = normalizeTimeDiff (diffClockTimes s t) < gap_minutes
             gap_minutes = TimeDiff 0 0 0 0 30 0 0 -- 30 minutes

         return $!
           [concat
              [ "Maximum users seen in ", G.showNick msg $ G.unpackNick who, ": "
              , show n
              , ", currently: ", show now
              , printf " (%0.1f%%)" (100 * (fromIntegral now    / fromIntegral n) :: Double)
              , ", active: ", show active
              , printf " (%0.1f%%)" (100 * (fromIntegral active / fromIntegral now) :: Double)
              ]
            ]

    process _ msg target _      rest = do
         (_,seenFM) <- readMS
         now        <- io getClockTime
         let (txt,safe) = first unlines (getAnswer msg rest seenFM now)
         if safe || not ("#" `isPrefixOf` G.nName target)
             then return [txt]
             else do lift $ ircPrivmsg (G.nick msg) txt
                     return []

    moduleInit _        = do
      wSFM <- bindModule2 withSeenFM
      zipWithM_ ircSignalConnect
        ["JOIN", "PART", "QUIT", "NICK", "353",      "PRIVMSG"] $ map wSFM
        [joinCB, partCB, quitCB, nickCB, joinChanCB, msgCB]

      -- This magically causes the 353 callback to be invoked :)
      -- this is broken...
      lift $ tryError $ send . G.names "freenode" . map G.nName =<< ircGetChannels

      -- and suck in our state. We read directly from the handle, to avoid copying

      c <- io $ findFile "seen"
      s <- io $ P.readFile c
      let ls = L.fromChunks [s]
      return (decode ls) >>= writeMS

    moduleExit _ = do
      chans <- lift $ ircGetChannels
      unless (null chans) $ do
            ct    <- io getClockTime
            modifyMS $ \(n,m) -> (n, botPart ct (map G.packNick chans) m)

        -- and write out our state:
      withMS $ \s _ -> io ( findFile "seen" >>= \ c -> encodeFile c s)

lcNick :: G.Nick -> G.Nick
lcNick (G.Nick svr nck) = G.Nick svr (lowerCaseString nck)

------------------------------------------------------------------------
getAnswer :: G.Message a => a -> String -> SeenMap -> ClockTime -> ([String], Bool)
getAnswer msg rest seenFM now
  | null nick' =
       let people  = map fst $ filter isActive $ M.toList seenFM
           isActive (_nick,state) = case state of
               (Present (Just (ct,_td)) _cs) -> recent ct
               _ -> False
           recent t = normalizeTimeDiff (diffClockTimes now t) < gap_minutes
           gap_minutes = TimeDiff 0 0 0 0 15 0 0
       in (["Lately, I have seen " ++ (if null people then "nobody"
                else listToStr "and" (map upAndShow people)) ++ "."], False)

  | pnick == G.lambdabotName msg =
        case M.lookup (G.packNick pnick) seenFM of
            Just (Present _ cs) ->
                (["Yes, I'm here. I'm in " ++ listToStr "and" (map upAndShow cs)], True)
            _ -> error "I'm here, but not here. And very confused!"

  | head (G.nName pnick) == '#' =
       let people  = map fst $ filter inChan $ M.toList seenFM
           inChan (_nick,state) = case state of
               (Present (Just _) cs)
                  -> G.packNick pnick `elem` cs
               _ -> False
       in (["In "++nick'++" I can see "
            ++ (if null people then "nobody"    -- todo, how far back does this go?
               else listToStr "and" (map upAndShow people)) ++ "."], False)

  | otherwise        = ((case M.lookup (G.packNick pnick) seenFM of
      Just (Present mct cs)            -> nickPresent mct (map upAndShow cs)
      Just (NotPresent ct td chans)    -> nickNotPresent ct td (map upAndShow chans)
      Just (WasPresent ct sw _ chans)  -> nickWasPresent ct sw (map upAndShow chans)
      Just (NewNick newnick)           -> nickIsNew newnick
      _ -> ircMessage ["I haven't seen ", nick, "."]), True)
  where
    -- I guess the only way out of this spagetty hell are printf-style responses.
    upAndShow = G.showNick msg . G.unpackNick
    nickPresent mct cs = ircMessage [
      if you then "You are" else nick ++ " is", " in ",
      listToStr "and" cs, ".",
      case mct of
        Nothing          -> concat [" I don't know when ", nick, " last spoke."]
        Just (ct,missed) -> prettyMissed (Stopped missed)
               (concat [" I last heard ", nick, " speak ",
                        lastSpoke {-, ", but "-}])
               (" Last spoke " ++ lastSpoke)
          where lastSpoke = clockDifference ct
     ]
    nickNotPresent ct missed chans = ircMessage [
       "I saw ", nick, " leaving ", listToStr "and" chans, " ",
       clockDifference ct, prettyMissed missed ", and " ""
     ]
    nickWasPresent ct sw chans = ircMessage [
       "Last time I saw ", nick, " was when I left ",
       listToStr "and" chans , " ", clockDifference ct,
       prettyMissed sw ", and " ""]
    nickIsNew newnick = ircMessage [if you then "You have" else nick++" has",
        " changed nick to ", us, "."] ++ fst (getAnswer msg us seenFM now)
      where

        findFunc pstr = case M.lookup pstr seenFM of
            Just (NewNick pstr') -> findFunc pstr'
            Just _               -> pstr
            Nothing              -> error "SeenModule.nickIsNew: Nothing"

        us = upAndShow $ findFunc newnick

    ircMessage = return . concat
    nick' = firstWord rest
    you   = pnick == lcNick (G.nick msg)
    nick  = if you then "you" else nick'
    pnick = lcNick $ G.readNick msg nick'
    clockDifference past
      | all (==' ') diff = "just now"
      | otherwise        = diff ++ " ago"
      where diff = timeDiffPretty . diffClockTimes now $ past

    prettyMissed (Stopped _) ifMissed _     = ifMissed ++ "."
    prettyMissed _           _ ifNotMissed  = ifNotMissed ++ "."

{-
    prettyMissed (Stopped missed) ifMissed _
      | missedPretty <- timeDiffPretty missed,
        any (/=' ') missedPretty
      = concat [ifMissed, "I have missed ", missedPretty, " since then."]

    prettyMissed _ _ ifNotMissed = ifNotMissed ++ "."
-}

-- | extract channels from message as packed, lower cased, strings.
msgChans :: G.Message a => a -> [Channel]
msgChans msg = map (G.packNick . lcNick) $ G.channels msg

-- | Callback for when somebody joins. If it is not the bot that joins, record
--   that we have a new user in our state tree and that we have never seen the
--   user speaking.
joinCB :: G.Message a => a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap
joinCB msg fm _ct nick
  | nick == (G.packNick $ G.lambdabotName msg) = Right fm
  | otherwise = Right $! insertUpd (updateJ Nothing chans) nick newInfo fm
  where newInfo = Present Nothing chans
        chans = msgChans msg

botPart :: ClockTime -> [Channel] -> SeenMap -> SeenMap
botPart ct cs fm = fmap botPart' fm where
    botPart' (Present mct xs) = case xs \\ cs of
        [] -> WasPresent ct (startWatch ct zeroWatch) mct cs
        ys -> Present mct ys
    botPart' (NotPresent ct' missed c)
        | head c `elem` cs = NotPresent ct' (startWatch ct missed) c
    botPart' (WasPresent ct' missed mct c)
        | head c `elem` cs = WasPresent ct' (startWatch ct missed) mct c
    botPart' us = us

-- | when somebody parts
partCB :: G.Message a => a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap
partCB msg fm ct nick
  | nick == (G.packNick $ G.lambdabotName msg) = Right $ botPart ct (msgChans msg) fm
  | otherwise      = case M.lookup nick fm of
      Just (Present mct xs) ->
        case xs \\ (msgChans msg) of
          [] -> Right $! M.insert nick
                 (NotPresent ct zeroWatch xs)
                 fm
          ys -> Right $! M.insert nick
                                 (Present mct ys)
                                 fm
      _ -> Left "someone who isn't known parted"

-- | when somebody quits
quitCB :: G.Message a => a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap
quitCB _ fm ct nick = case M.lookup nick fm of
    Just (Present _ct xs) -> Right $! M.insert nick (NotPresent ct zeroWatch xs) fm
    _ -> Left "someone who isn't known has quit"

-- | when somebody changes his\/her name
nickCB :: G.Message a => a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap
nickCB msg fm _ nick = case M.lookup nick fm of
   Just status -> let fm' = M.insert nick (NewNick lcnewnick) fm
                  in  Right $! M.insert lcnewnick status fm'
   _           -> Left "someone who isn't here changed nick"
   where
   newnick = drop 1 $ head (G.body msg)
   lcnewnick = G.packNick $ lcNick $ G.readNick msg newnick

-- use IRC.IRC.channels?
-- | when the bot join a channel
joinChanCB :: G.Message a => a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap
joinChanCB msg fm now _nick
    = Right $ fmap (updateNP now chan) $ foldl insertNick fm chanUsers
  where
    l = G.body msg
    chan = G.packNick $ lcNick $ G.readNick msg $ l !! 2
    chanUsers = map (G.packNick . lcNick . G.readNick msg) $ words (drop 1 (l !! 3)) -- remove ':'
    insertNick fm' u = insertUpd (updateJ (Just now) [chan])
                                    (G.packNick . unUserMode . lcNick . G.unpackNick $ u)
                                    (Present Nothing [chan])
                                    fm'

-- | when somebody speaks, update their clocktime
msgCB :: G.Message a => a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap
msgCB _ fm ct nick =
  case M.lookup nick fm of
    Just (Present _ xs) -> Right $!
      M.insert nick (Present (Just (ct, noTimeDiff)) xs) fm
    _ -> Left "someone who isn't here msg us"

-- misc. functions
unUserMode :: G.Nick -> G.Nick
unUserMode nick = G.Nick (G.nTag nick) (dropWhile (`elem` "@+") $ G.nName nick)

-- | Callbacks are only allowed to use a limited knowledge of the world.
-- 'withSeenFM' is (up to trivial isomorphism) a monad morphism from the
-- restricted
--   'ReaderT (IRC.Message, ClockTime, Nick) (StateT SeenState (Error String))'
-- to the
--   'ReaderT IRC.Message (Seen IRC)'
-- monad.
withSeenFM :: G.Message a
           => (a -> SeenMap -> ClockTime -> Nick -> Either String SeenMap)
           -> a
           -> Seen LB ()

withSeenFM f msg = do
    let nick = G.packNick . lcNick . G.nick $ msg
    withMS $ \(maxUsers,state) writer -> do
      ct <- io getClockTime
      case f msg state ct nick of
          Left _         -> return () -- debugStrLn $ "SeenModule> " ++ err
          Right newstate -> do

                let curUsers = length $! [ () | (_,Present _ chans) <- M.toList state
                                         , P.pack chan `elem` chans ]

                    newMax = case M.lookup chan maxUsers of
                        Nothing -> M.insert chan curUsers maxUsers
                        Just  n -> if n < curUsers
                                        then M.insert chan curUsers maxUsers
                                        else maxUsers

                newMax `seq` newstate `seq` writer (newMax, newstate)

    where chan = P.unpack . G.packNick . lcNick . head . G.channels $! msg

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
updateJ (Just now) cs (WasPresent lastSeen _ (Just (lastSpoke, missed)) channels)
  | head channels `elem` cs
  ---                 newMissed
  --- |---------------------------------------|
  --- |-------------------|                   |
  ---        missed    lastSeen              now
  = let newMissed = addToClockTime missed now `diffClockTimes` lastSeen
    in  newMissed `seq` Present (Just (lastSpoke, newMissed)) cs
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

------------------------------------------------------------------------
-- Stop watches mini-library --

zeroWatch :: StopWatch
zeroWatch = Stopped noTimeDiff

startWatch :: ClockTime -> StopWatch -> StopWatch
startWatch now (Stopped td) = Running $! td `addToClockTime` now
startWatch _ alreadyStarted = alreadyStarted

stopWatch :: ClockTime -> StopWatch -> StopWatch
stopWatch now (Running t)  = Stopped $! t `diffClockTimes` now
stopWatch _ alreadyStopped = alreadyStopped
