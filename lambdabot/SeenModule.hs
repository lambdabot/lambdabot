module SeenModule (theModule) where

import IRC
import Util
import Config
import qualified Map as M

import Data.List ((\\),nub)

import Control.Monad (when)
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
joinCB msg = do
        let nick = ircnick msg
        when (nick /= name config) $
           withSeenFM $ \fm -> do
               let newInfo = Present Nothing (ircchannel msg)
                   fm' = M.insertWith updateJ (lowerCaseString nick) newInfo fm
               writeMS fm'


partCB :: IRCMessage -> Seen IRC () -- when somebody parts
partCB msg
  = do let myname = name config
       withSeenFM $ \fm ->
        if nick == myname then -- when the bot parts
          do l <- mapM (botPart $ ircchannel msg) (M.toList fm)
	     writeMS $ M.fromList l
        else case (M.lookup lcnick fm) of
                Just (Present mct xs) ->
	         do case xs \\ (ircchannel msg) of
		        [] -> do ct <- time
			         let fm' = M.insert lcnick
					     (NotPresent ct (listToStr "and" xs)) fm
			         writeMS fm' 
			ys -> writeMS $ M.insert lcnick 
						(Present mct ys) fm
                _ -> debugStrLn "SeenModule> someone who isn't known parted"
    where nick = unUserMode (ircnick msg)
          lcnick = lowerCaseString nick
          botPart cs (nick', us) 
            = case us of
		  Present mct xs -> 
		      case xs \\ cs of
			  [] -> do ct <- time
				   return (nick', WasPresent ct (listToStr "and" cs))
			  ys -> return (nick', Present mct ys)
		  _other -> return (nick', us)

quitCB :: IRCMessage -> Seen IRC () -- when somebody quits
quitCB msg
  = withSeenFM $ \fm -> 
    do ct <- time
       let nick = unUserMode (ircnick msg)
           lcnick = lowerCaseString nick
       case (M.lookup lcnick fm) of
           Just (Present _ct xs) -> 
               let fm' = M.insert lcnick
                               (NotPresent ct (head xs)) fm
                   in writeMS fm'
           _ -> debugStrLn "SeenModule> someone who isn't known has quit"

nickCB :: IRCMessage -> Seen IRC () -- when somebody changes his/her name
nickCB msg 
  = withSeenFM $ \fm ->
    case (M.lookup lcnick fm) of
        Just (Present mct xs) -> 
            let fm' = M.insert lcnick (NewNick newnick) fm
                fm'' = M.insert lcnewnick (Present mct xs) fm'
                in writeMS fm''
        _ -> debugStrLn "SeenModule> someone who isn't here changed nick"
    where newnick = drop 1 $ head (msgParams msg)
          lcnewnick = lowerCaseString newnick
          nick = ircnick msg
          lcnick = lowerCaseString nick

joinChanCB :: IRCMessage -> Seen IRC () -- when the bot join a channel
joinChanCB msg
  = withSeenFM $ \fm -> do
    let l = msgParams msg
        chan = l !! 2
        chanUsers = words (drop 1 (l !! 3)) -- remove ':'
        fooFunc fm' u = M.insertWith updateJ
                              (lowerCaseString $ unUserMode u) (Present Nothing [chan]) fm'
        seenFM' = foldl fooFunc fm chanUsers
    writeMS seenFM'

-- when somebody speaks, update their clocktime
msgCB :: IRCMessage -> Seen IRC ()
msgCB msg
    = withSeenFM $ \fm ->
        case M.lookup (lowerCaseString nick) fm of
            Just (Present _ct xs) -> do
                ct <- time
                let fm' = M.insert (lowerCaseString nick) (Present (Just ct) xs) fm
                writeMS fm'
            _ -> debugStrLn "SeenModule> someone who isn't here msg us"
    where nick = ircnick msg

-- misc. functions
unUserMode :: Nick -> Nick
unUserMode nick = dropWhile (`elem` "@+") nick

withSeenFM :: (SeenState -> Seen IRC ()) -> Seen IRC ()
withSeenFM f = f =<< readMS

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



