--
-- Copyright (c) 2004 Thomas Jaeger
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | Keep track of IRC users.
--
module Plugins.Seen (theModule) where

import AltTime
import Binary
import Config
import ErrorUtils          (tryError)
import LBState
import Lambdabot
import Serial  ({-instances-})
import Util (lowerCaseString, firstWord, listToStr, debugStrLn)
import qualified IRC

import qualified Map as M
import qualified Data.FastPackedString as P

import Data.List           ((\\), nub)

import System.IO
import System.Directory

import System.Time (normalizeTimeDiff) -- or export from AltTime.hs?

import Control.Monad       (unless, zipWithM_)
import Control.Monad.Trans (liftIO, MonadIO)

------------------------------------------------------------------------

newtype SeenModule = SeenModule ()

theModule :: MODULE
theModule = MODULE $ SeenModule ()

-- Try using packed strings?

-- | The type of channels
type Channel = P.FastString

-- | The type of nicknames
type Nick = P.FastString

-- | We last heard the user speak at ClockTime; since then we have missed
--   TimeDiff of him because we were absent.
type LastSpoke = Maybe (ClockTime, TimeDiff)

-- | 'UserStatus' keeps track of the status of a given Nick name.
data UserStatus
        = Present LastSpoke [Channel]  
          -- ^ Records when the nick last spoke and that the nick is currently 
          --   in [Channel].
        | NotPresent ClockTime StopWatch [Channel]
          -- ^ The nick is not present and was last seen at ClockTime in Channel.
          --   The second argument records how much we've missed.
        | WasPresent ClockTime StopWatch LastSpoke [Channel]
          -- ^ The bot parted a channel where the user was. The Clocktime
          --   records the time and Channel the channel this happened in.
          --   We also save the reliablility of our information and the
          --   time we last heard the user speak.
        | NewNick Nick                 
          -- ^ The user changed nick to something new.
    deriving (Show, Read)

data StopWatch = Stopped TimeDiff 
               | Running ClockTime 
        deriving (Show,Read)

type SeenState = M.Map Nick UserStatus
type Seen m a = ModuleT SeenState m a
 
------------------------------------------------------------------------

-- ok, since this module generates quite a lot of state, what we'll do
-- is use Binary to pack this value, since Read is sooo slow and exe (as
-- my gf says :)

instance Binary (M.Map Nick UserStatus) where
    put_ bh m = put_ bh (M.toList m)
    get bh    = do x <- get bh ; return (M.fromList x)

instance Binary StopWatch where
    put_ bh (Stopped td) = do
        putByte bh 0
        put_ bh td

    put_ bh (Running ct) = do
        putByte bh 1
        put_ bh ct

    get bh = do 
        h <- getWord8 bh
        case h of
                0 -> do x <- get bh ; return (Stopped x)
                1 -> do x <- get bh ; return (Running x)
                _ -> error "Seen.StopWatch.get"

instance Binary UserStatus where
    put_ bh (Present spoke chans) = do
        putByte bh 0
        put_ bh spoke
        put_ bh chans
    put_ bh (NotPresent ct sw chans) = do
        putByte bh 1
        put_ bh ct
        put_ bh sw
        put_ bh chans
    put_ bh (WasPresent ct sw spoke chans) = do
        putByte bh 2
        put_ bh ct
        put_ bh sw
        put_ bh spoke
        put_ bh chans
    put_ bh (NewNick n) = do
        putByte bh 3
        put_ bh n

    get bh = do
        h <- getWord8 bh
        case h of
            0 -> do
                x <- get bh
                y <- get bh
                return (Present x y)
            1 -> do
                x <- get bh
                y <- get bh
                z <- get bh
                return (NotPresent x y z)
            2 -> do
                x <- get bh
                y <- get bh
                z <- get bh
                a <- get bh
                return (WasPresent x y z a)
            3 -> do 
                x <- get bh
                return (NewNick x)

            _ -> error "Seen.UserStatus.get"

------------------------------------------------------------------------

instance Module SeenModule SeenState where
    moduleHelp _ _      = "Report if a user has been seen by the bot"
    moduleCmds _        = ["seen"]
    moduleDefState _    = return M.empty

    moduleInit _        = do 
      zipWithM_ ircSignalConnect 
        ["JOIN", "PART", "QUIT", "NICK", "353",      "PRIVMSG"] $ map withSeenFM
        [joinCB, partCB, quitCB, nickCB, joinChanCB, msgCB]
      -- This magically causes the 353 callback to be invoked :)
      tryError $ send . IRC.names =<< ircGetChannels

      -- and suck in our state:
      s  <- liftIO $ do 
              b <- doesFileExist "State/seen"
              if b 
                then do
                  h  <- openFile "State/seen" ReadMode
                  bh <- openBinIO_ h
                  st <- {-# SCC "Seen.get" #-} get bh
                  hClose h
                  return st
                else return M.empty -- if not, construct a default state.
      writeMS s
      return ()
    
    moduleExit _ = do
      chans <- ircGetChannels
      unless (null chans) $ do
            ct    <- liftIO getClockTime
            modifyMS $ botPart ct (map P.pack chans)

        -- and write out our state:
      s <- readMS
      liftIO $ do 
              h  <- openFile "State/seen" WriteMode
              bh <- openBinIO_ h
              {-# SCC "Seen.get" #-}put_ bh s
              hClose h
      return ()
    
    process _ msg _ _ rest = do 
         seenFM <- readMS
         now    <- liftIO getClockTime
         return [unlines $ getAnswer msg rest seenFM now]

------------------------------------------------------------------------
-- | The bot's name, lowercase
myname :: String
myname = lowerCaseString (name config)

getAnswer :: IRC.Message -> String -> SeenState -> ClockTime -> [String]
getAnswer msg rest seenFM now 
  | null lcnick = 
       let people  = map fst $ filter isActive $ M.toList seenFM
           isActive (_nick,state) = case state of 
               (Present (Just (ct,_td)) _cs) -> recent ct
               _ -> False
           recent t = normalizeTimeDiff (diffClockTimes now t) < gap_minutes
           gap_minutes = TimeDiff 0 0 0 0 15 0 0
       in ["Lately, I have seen " ++ (if null people then "nobody" 
               else listToStr "and" (map P.unpack people)) ++ "."]

  | lcnick == myname = 
        case M.lookup (P.pack lcnick) seenFM of
            Just (Present _ cs) -> 
                ["Yes, I'm here. I'm in " ++ listToStr "and" (map P.unpack cs)]
            _ -> error "I'm here, but not here. And very confused!"

  | length lcnick > 0 && head lcnick == '#' =
       let channel = lcnick
           people  = map fst $ filter inChan $ M.toList seenFM
           inChan (_nick,state) = case state of 
               (Present (Just _) cs) 
                  -> P.pack channel `elem` cs
               _ -> False
       in ["In "++channel++" I can see "
            ++ (if null people then "nobody"    -- todo, how far back does this go?
               else listToStr "and" (map P.unpack people)) ++ "."]

  | otherwise        = case M.lookup (P.pack lcnick) seenFM of
      Just (Present mct cs)            -> nickPresent mct (map P.unpack cs)
      Just (NotPresent ct td chans)    -> nickNotPresent ct td (map P.unpack chans)
      Just (WasPresent ct sw _ chans)  -> nickWasPresent ct sw (map P.unpack chans)
      Just (NewNick newnick)           -> nickIsNew (P.unpack newnick)
      _ -> ircMessage ["I haven't seen ", nick, "."]
  where
    -- I guess the only way out of this spagetty hell are printf-style responses.
    nickPresent mct cs = ircMessage [
      if you then "You are" else nick ++ " is", " in ",
      listToStr "and" cs, ".",
      case mct of
        Nothing          -> concat [" I don't know when ", nick, " last spoke."]
        Just (ct,missed) -> prettyMissed (Stopped missed)
               (concat [" I last heard ", nick, " speak ", 
                        lastSpoke, ", but "])
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
        " changed nick to ", us, "."] ++ getAnswer msg us seenFM now 
      where

        findFunc pstr = case M.lookup pstr seenFM of
            Just (NewNick pstr') -> findFunc pstr'
            Just _               -> pstr
            Nothing              -> error "SeenModule.nickIsNew: Nothing"

        us = P.unpack $ findFunc (P.pack $ lowerCaseString newnick)

    ircMessage = return . concat
    nick' = firstWord rest
    you   = nick' == IRC.nick msg
    nick  = if you then "you" else nick'
    lcnick = lowerCaseString nick'
    clockDifference past 
      | all (==' ') diff = "just now"
      | otherwise        = diff ++ " ago" 
      where diff = timeDiffPretty . diffClockTimes now $ past
    prettyMissed (Stopped missed) ifMissed _
      | missedPretty <- timeDiffPretty missed, 
        any (/=' ') missedPretty
      = concat [ifMissed, "I have missed ", missedPretty, " since then."]
    prettyMissed _ _ ifNotMissed = ifNotMissed ++ "."


-- | Callback for when somebody joins. If it is not the bot that joins, record
--   that we have a new user in our state tree and that we have never seen the
--   user speaking.
joinCB :: IRC.Message -> SeenState -> ClockTime -> Nick -> Either String SeenState
joinCB msg fm _ct nick
  | nick == (P.pack myname) = Right fm
  | otherwise               = Right $ M.insertUpd (updateJ Nothing (map P.pack $ IRC.channels msg)) 
                                         nick newInfo fm
  where newInfo = Present Nothing (map P.pack $ IRC.channels msg)


botPart :: ClockTime -> [Channel] -> SeenState -> SeenState
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
partCB :: IRC.Message -> SeenState -> ClockTime -> Nick -> Either String SeenState
partCB msg fm ct nick
  | nick == (P.pack myname) = Right $ botPart ct (map P.pack $ IRC.channels msg) fm
  | otherwise      = case M.lookup nick fm of
      Just (Present mct xs) ->
        case xs \\ (map P.pack $ IRC.channels msg) of
          [] -> Right $ M.insert nick
                 (NotPresent ct zeroWatch xs)
                 fm
          ys -> Right $ M.insert nick
                                 (Present mct ys)
                                 fm
      _ -> Left "someone who isn't known parted"

-- | when somebody quits
quitCB :: IRC.Message -> SeenState -> ClockTime -> Nick -> Either String SeenState 
quitCB _ fm ct nick = case M.lookup nick fm of
    Just (Present _ct xs) -> Right $ M.insert nick (NotPresent ct zeroWatch xs) fm
    _ -> Left "someone who isn't known has quit"

-- | when somebody changes his\/her name
nickCB :: IRC.Message -> SeenState -> ClockTime -> Nick -> Either String SeenState
nickCB msg fm _ nick = case M.lookup nick fm of
   Just status -> let fm' = M.insert nick (NewNick $ P.pack newnick) fm
                  in  Right $ M.insert lcnewnick status fm'
   _           -> Left "someone who isn't here changed nick"
   where
   newnick = drop 1 $ head (msgParams msg)
   lcnewnick = P.pack $ lowerCaseString newnick

-- use IRC.IRC.channels?
-- | when the bot join a channel
joinChanCB :: IRC.Message -> SeenState -> ClockTime -> Nick -> Either String SeenState
joinChanCB msg fm now _nick 
    = Right $ fmap (updateNP now chan) $ foldl insertNick fm chanUsers
  where
    l = msgParams msg
    chan = P.pack $ l !! 2
    chanUsers = map P.pack $ words (drop 1 (l !! 3)) -- remove ':'
    insertNick fm' u = M.insertUpd (updateJ (Just now) [chan])
                                    (P.pack . lowerCaseString . P.unpack .  unUserMode $ u)
                                    (Present Nothing [chan])
                                    fm'

-- | when somebody speaks, update their clocktime
msgCB :: IRC.Message -> SeenState -> ClockTime -> Nick -> Either String SeenState
msgCB _ fm ct nick =
  case M.lookup nick fm of
    Just (Present _ xs) -> Right $ 
      M.insert nick (Present (Just (ct, noTimeDiff)) xs) fm
    _ -> Left "someone who isn't here msg us"

-- misc. functions
unUserMode :: Nick -> Nick
unUserMode nick = P.dropWhile (`elem` "@+") nick

-- | Callbacks are only allowed to use a limited knowledge of the world. 
-- 'withSeenFM' is (up to trivial isomorphism) a monad morphism from the 
-- restricted
--   'ReaderT (IRC.Message, ClockTime, Nick) (StateT SeenState (Error String))'
-- to the
--   'ReaderT IRC.Message (Seen IRC)'
-- monad.
withSeenFM :: (IRC.Message -> SeenState -> ClockTime -> Nick
                  -> Either String SeenState)
              -> IRC.Message
              -> Seen LB ()
withSeenFM f msg = do 
    let nick = P.pack . lowerCaseString . P.unpack . unUserMode . P.pack . IRC.nick $ msg
    withMS $ \state writer -> do
      ct <- liftIO getClockTime
      case f msg state ct nick of
          Right newstate -> writer newstate
          Left err -> debugStrLn $ "SeenModule> " ++ err

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
startWatch now (Stopped td) = Running $ td `addToClockTime` now
startWatch _ alreadyStarted = alreadyStarted

stopWatch :: ClockTime -> StopWatch -> StopWatch
stopWatch now (Running t)  = Stopped $ t `diffClockTimes` now
stopWatch _ alreadyStopped = alreadyStopped

