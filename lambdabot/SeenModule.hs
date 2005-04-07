module SeenModule (theModule) where

import IRC
import Util
import Config
import qualified Map as M

import Data.List ((\\),nub)

import Control.Monad.Trans (liftIO, MonadIO)

import System.Time

------------------------------------------------------------------------

newtype SeenModule = SeenModule ()

theModule :: MODULE
theModule = MODULE $ SeenModule ()

type Channel = String
type Nick = String

data UserStatus
        = Present (Maybe ClockTime) [Channel] -- last spoke, the user is in [Channel]
        | NotPresent ClockTime Channel -- the user was last seen in Channel
        | WasPresent ClockTime Channel -- if the bot parted Channel where a user was
        | NewNick Nick                 -- the user has a new nick
    deriving Show

type SeenState = M.Map Nick UserStatus
type Seen m a = ModuleT SeenState m a

instance Module SeenModule SeenState where
    moduleName _        = "seen"
    moduleHelp _ _      = return "Report if a user has been seen by the bot"
    moduleCmds _        = return ["seen"]
    moduleDefState _    = return M.empty
    moduleInit m
      = do ircSignalConnect m "JOIN" $ joinCB
           ircSignalConnect m "PART" $ partCB
           ircSignalConnect m "QUIT" $ quitCB
           ircSignalConnect m "NICK" $ nickCB
           ircSignalConnect m "353" $ joinChanCB
           ircSignalConnect m "PRIVMSG" $ msgCB

    process m msg target cmd rest =
      do seenFM <- readMS
	 now <- time
         myname <- return $ lowerCaseString (name config)
         let nick = firstWord rest
             lcnick = lowerCaseString nick
	     ircMessage = ((ircPrivmsg target) . concat)
	     clockDifference = timeDiffPretty . (diffClockTimes now)
             nickPresent mct cs =
               do ircPrivmsg target $
                     concat [nick, " is in ", listToStr "and" cs, ".",
			     case mct of
				Nothing -> concat
				            [" I don't know when ",
					     nick,
					     " last spoke."]
			        Just ct -> concat
			                    [" Last spoke ",
					     let when' = clockDifference ct
					     in if null when'
					          then "just now."
					          else when']]
             nickNotPresent ct chan =
	       do ircMessage ["I saw ", nick, " leaving ", chan, " ",
			      clockDifference ct, "ago."]
             nickWasPresent ct chan =
	       do ircMessage ["Last time I saw ", nick, "was when I left ",
			      chan , " ", clockDifference ct, "ago."]
             nickIsNew newnick =
               do let findFunc str =
                        case M.lookup (lowerCaseString str) seenFM of
	                  Just (NewNick str') -> findFunc str'
	                  Just _              -> str
                          Nothing             -> error "SeenModule.nickIsNew: Nothing"
		      us = findFunc newnick
                  ircMessage [nick, " has changed nick to ", us, "."]
                  process m msg target cmd us
         if lcnick == myname
            then ircPrivmsg target "Yes, I'm here"
            else case M.lookup lcnick seenFM of
                  Just (Present mct cs) -> nickPresent mct cs
                  Just (NotPresent ct chan) -> nickNotPresent ct chan
                  Just (WasPresent ct chan) -> nickWasPresent ct chan
                  Just (NewNick newnick) -> nickIsNew newnick
                  _ -> ircPrivmsg target $ "I haven't seen " ++ nick

joinCB :: IRCMessage -> Seen IRC () -- when somebody joins
joinCB msg = withSeenFM $ \fm _ct myname ->
  let nick = ircnick msg
  in if nick /= myname
       then let newInfo = Present Nothing (ircchannel msg)
            in  Left $ M.insertWith updateJ (lowerCaseString nick) newInfo fm
       else Left fm

partCB :: IRCMessage -> Seen IRC () -- when somebody parts
partCB msg = withSeenFM $ \fm ct myname ->
  let nick = unUserMode (ircnick msg)
      lcnick = lowerCaseString nick
      botPart cs (nick', us) =
        case us of
          Present mct xs ->
            case xs \\ cs of
              [] -> (nick', WasPresent ct (listToStr "and" cs))
              ys -> (nick', Present mct ys)
          _ -> (nick', us)
  in if nick == myname
       then let l = map (botPart $ ircchannel msg) (M.toList fm)
            in Left $ M.fromList l
       else case M.lookup lcnick fm of
              Just (Present mct xs) ->
                case xs \\ (ircchannel msg) of
                  [] -> Left $ M.insert lcnick
			                (NotPresent ct (listToStr "and" xs))
					fm
                  ys -> Left $ M.insert lcnick
                                        (Present mct ys)
					fm
	      _ -> Right "SeenModule> someone who isn't known parted"

quitCB :: IRCMessage -> Seen IRC () -- when somebody quits
quitCB msg = withSeenFM $ \fm ct _myname ->
  let nick = unUserMode (ircnick msg)
      lcnick = lowerCaseString nick
  in case M.lookup lcnick fm of
       Just (Present _ct xs) ->
         Left $ M.insert lcnick (NotPresent ct (head xs)) fm
       _ -> Right "SeenModule> someone who isn't known has quit"

nickCB :: IRCMessage -> Seen IRC () -- when somebody changes his/her name
nickCB msg = withSeenFM $ \fm _ct _myname ->
  let newnick = drop 1 $ head (msgParams msg)
      lcnewnick = lowerCaseString newnick
      nick = ircnick msg
      lcnick = lowerCaseString nick
  in case M.lookup lcnick fm of
       Just (Present mct xs) ->
         let fm' = M.insert lcnick (NewNick newnick) fm
         in Left $ M.insert lcnewnick (Present mct xs) fm'
       _ -> Right "SeenModule> someone who isn't here changed nick"

joinChanCB :: IRCMessage -> Seen IRC () -- when the bot join a channel
joinChanCB msg = withSeenFM $ \fm _ct _myname ->
  let l = msgParams msg
      chan = l !! 2
      chanUsers = words (drop 1 (l !! 3)) -- remove ':'
      insertNick fm' u = M.insertWith updateJ
                                      (lowerCaseString $ unUserMode u)
                                      (Present Nothing [chan])
                                      fm'
      in Left $ foldl insertNick fm chanUsers

-- when somebody speaks, update their clocktime
msgCB :: IRCMessage -> Seen IRC ()
msgCB msg = withSeenFM $ \fm ct _myname ->
  let nick = ircnick msg
  in case M.lookup (lowerCaseString nick) fm of
       Just (Present _ct xs) ->
         Left $ M.insert (lowerCaseString nick) (Present (Just ct) xs) fm
       _ -> Right "SeenModule> someone who isn't here msg us"

-- misc. functions
unUserMode :: Nick -> Nick
unUserMode nick = dropWhile (`elem` "@+") nick


withSeenFM :: (SeenState -> ClockTime -> String
	       -> Either SeenState String) -> Seen IRC ()
withSeenFM f = do state <- readMS
                  ct <- time
                  myname <- return $ (lowerCaseString . name) config
                  case f state ct myname of
                    Left newstate -> writeMS newstate
                    Right err -> debugStrLn err

updateJ :: UserStatus -> UserStatus -> UserStatus
updateJ = flip updateJ' where
  updateJ' (Present _ct cs) (Present ct c) = Present ct $ nub (c ++ cs)
  updateJ' _x y@(Present _ct _cs) = y
  updateJ' x _ = x

time :: MonadIO m => m ClockTime
time = liftIO getClockTime

ircchannel :: IRCMessage -> [Channel]
ircchannel msg
  = let cstr = head $ msgParams msg
    in map (\(x:xs) -> if x == ':' then xs else (x:xs)) (split "," cstr)
           -- solves what seems to be an inconsistency in the parser


-- annoying
timeDiffPretty :: TimeDiff -> String
timeDiffPretty td =
  let secs = tdSec td
      mins = secs `div` 60
      hours = mins `div` 60
      days = hours `div` 24
      months = days `div` 28
      years = months `div` 12
  in foldr1 (++) [foo years "year",
                  foo (months `mod` 12) "month",
                  foo (days `mod` 28) "day",
                  foo (hours `mod` 24) "hour",
                  foo (mins `mod` 60) "minute",
                  foo (secs `mod` 60) "second"]
                   where foo i str
                             | i > 0 = if i == 1 then "1 " ++ str ++ " "
                                                 else (show i) ++ " " ++ str ++ "s "
                             | otherwise = []



