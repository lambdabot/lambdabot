{-# OPTIONS -cpp -fglasgow-exts #-}

module SeenModule 
    (seenModule,
     theModule,
     SeenModule) 
where

import IRC
import Util
import BotConfig
import qualified Map as M

import Data.FiniteMap
import Control.Monad.State
import Data.Dynamic
import Data.IORef
import Time
import List ((\\),nub)

newtype SeenModule = SeenModule ()

theModule = MODULE seenModule
seenModule = SeenModule ()

type Channel = String
type Nick = String

data UserStatus = Present [Channel]            -- the user is in [Channel]
                | NotPresent ClockTime Channel -- the user was last seen in Channel
                | WasPresent ClockTime Channel -- if the bot parted Channel where a user was
                | NewNick Nick                 -- the user has a new nick
                deriving Show

ctCon = mkTyCon "ClockTime"

#if __GLASGOW_HASKELL__ < 603
myTy = mkAppTy
#else
myTy = mkTyConApp
#endif

instance Typeable ClockTime where
    typeOf _ = myTy ctCon []

usCon = mkTyCon "UserStatus"
instance Typeable UserStatus where
    typeOf _ = myTy usCon []

instance Module SeenModule where
    moduleName _        = return "seen"
    moduleSticky _      = False
    commands _          = return ["seen"]
    moduleInit _ 
      = do s <- get
           newRef <-
               liftIO . newIORef $
                      ModuleState (emptyFM :: FiniteMap Nick UserStatus)
           let stateMap = ircModuleState s
           put (s { ircModuleState =
                    M.insert "seen" newRef stateMap })
           ircSignalConnect "JOIN" $ joinCB 
           ircSignalConnect "PART" $ partCB 
           ircSignalConnect "QUIT" $ quitCB
           ircSignalConnect "NICK" $ nickCB
           ircSignalConnect "353" $ joinChanCB

    process m msg target cmd rest 
      = do seenRef <- gets (\s -> M.lookup "seen" (ircModuleState s))
           myname <- getMyname
           case seenRef of
               Just seenFMRef ->
                   do seenFMState <- liftIO $ readIORef seenFMRef
                      let (seenFM :: FiniteMap Nick UserStatus) 
                              = stripMS seenFMState
                          nick = takeWhile (/=' ') rest
                      if nick == myname then ircPrivmsg target "Yes, I'm here"
                         else case lookupFM seenFM nick of 
                               Just (Present cs) ->
                                   ircPrivmsg target $ nick ++ " is in " ++ 
                                                  (listToStr cs) ++ "."
                               Just (NotPresent ct chan) ->
                                   do now <- time
                                      ircPrivmsg target $ 
                                                     "I saw " ++ nick ++ 
                                                     " leaving " ++ 
                                                     chan ++ " " ++ 
                                                     (toPretty $ 
                                                      diffClockTimes now ct) ++
                                                     "ago."
                               Just (WasPresent ct chan) ->
                                   do now <- time
                                      ircPrivmsg target $
                                                     "Last time I saw " ++ 
                                                     nick ++ 
                                                     " was when I left " ++
                                                     chan ++ " " ++ 
                                                     (toPretty $ 
                                                      diffClockTimes now ct) ++
                                                     "ago."
                               Just (NewNick newnick) ->
                                   do let findfunc str = 
                                            case (lookupFM seenFM str) of
                                                Just (NewNick str') -> 
							findfunc str'
                                                Just x -> 
							str
                                          us = findfunc newnick
                                      ircPrivmsg target $
                                                        nick ++ 
                                                        " has changed nick to "
                                                        ++ us ++ "." 
                                      process m msg target cmd us
                               _ -> ircPrivmsg target $ "I haven't seen " 
                                                        ++ nick
               _ -> liftLB (moduleInit m) >> process m msg target cmd rest

joinCB :: IRCMessage -> IRC () -- when somebody joins
joinCB msg 
  = do myname <- getMyname
       when (nick /= myname) $
        withSeenFM $ \fm ref ->
        let newInfo = Present (ircchannel msg)
            fm' = addToFM_C updateJ fm nick newInfo
            in (setSeenFM ref fm')
    where nick = ircnick msg

partCB :: IRCMessage -> IRC () -- when somebody parts
partCB msg 
  = do myname <- getMyname
       withSeenFM $ \fm ref ->
        if nick == myname then -- when the bot parts
          do l <- mapM (botPart $ ircchannel msg) (fmToList fm)
	     setSeenFM ref (listToFM l)
        else case (lookupFM fm nick) of
                Just (Present xs) -> 
	         do case xs \\ (ircchannel msg) of 
		        [] -> do ct <- time 
			         let fm' = addToFM_C (\_ x -> x) fm nick 
					     (NotPresent ct (listToStr xs))
			         setSeenFM ref fm' 
			ys -> setSeenFM ref $ addToFM_C (\_ x -> x) fm nick 
						(Present ys)  
                _ -> debugStrLn "SeenModule> someone who isn't known parted"
    where nick = unUserMode (ircnick msg)
	  botPart cs (nick, us) 
            = case us of
		  Present xs -> 
		      case xs \\ cs of
			  [] -> do ct <- time
				   return (nick, WasPresent ct (listToStr cs))
			  ys -> return (nick, Present ys)
		  other -> return (nick, us)

quitCB :: IRCMessage -> IRC () -- when somebody quits
quitCB msg
  = withSeenFM $ \fm ref -> 
    do ct <- time
       let nick = unUserMode (ircnick msg)
       case (lookupFM fm nick) of
           Just (Present xs) -> 
               let fm' = addToFM_C (\_ x -> x) fm nick 
                               (NotPresent ct (head xs))
                   in setSeenFM ref fm'
           _ -> debugStrLn "SeenModule> someone who isn't known has quit"

nickCB :: IRCMessage -> IRC () -- when somebody changes his/her name
nickCB msg 
  = withSeenFM $ \fm ref ->
    case (lookupFM fm nick) of
        Just (Present xs) -> 
            let fm' = addToFM_C (\_ x -> x) fm nick (NewNick newnick)
                fm'' = addToFM fm' newnick (Present xs)
                in setSeenFM ref fm''
        _ -> debugStrLn "SeenModule> someone who isn't here changed nick"
    where newnick = drop 1 $ head (msgParams msg)
          nick = ircnick msg

joinChanCB :: IRCMessage -> IRC () -- when the bot join a channel
joinChanCB msg
  = withSeenFM $ \fm ref ->
    let l = msgParams msg
        chan = l !! 2
        chanUsers = words (drop 1 (l !! 3)) -- remove ':'
        fooFunc fm u = addToFM_C updateJ fm 
                              (unUserMode u) (Present [chan])
        seenFM' = foldl fooFunc fm chanUsers
        in setSeenFM ref seenFM'

-- misc. functions
unUserMode :: Nick -> Nick
unUserMode nick = dropWhile (`elem` "@+") nick

withSeenFM :: (FiniteMap Nick UserStatus -> IORef ModuleState -> IRC ()) -> IRC ()
withSeenFM f = 
  do maybeSeenFM <- getSeenFM
     case maybeSeenFM of 
         Just (fm, ref) -> f fm ref
         _ -> debugStrLn "SeenModule> Couldn't lookup the user database"
     where getSeenFM 
             = do seenRef <- gets (\s -> M.lookup "seen" (ircModuleState s))
                  case seenRef of
                      Just seenFMRef ->
                          do seenFMState <- liftIO $ readIORef seenFMRef
                             let (seenFM :: FiniteMap Nick UserStatus)
                                     = stripMS seenFMState
                                 in return (Just (seenFM, seenFMRef))
                      _ -> return Nothing

setSeenFM :: IORef ModuleState -> FiniteMap Nick UserStatus -> IRC ()
setSeenFM ref fm = liftIO $ writeIORef ref $ ModuleState fm

updateJ :: UserStatus -> UserStatus -> UserStatus
updateJ (Present cs) (Present c) = Present $ nub (c ++ cs)              
updateJ x y@(Present cs) = y
updateJ x _ = x

time :: IRC ClockTime
time = liftIO getClockTime

ircchannel :: IRCMessage -> [Channel]
ircchannel msg
  = let cstr = head $ msgParams msg
        splitter [] = []
        splitter xs = let (chan, rest) = break (==',') xs
                          in chan : splitter (drop 1 rest) -- get rid of ','
        in map (\(x:xs) -> if x == ':' then xs else (x:xs)) (splitter cstr)
                -- solves what seems to be an inconsistency in the parser

listToStr :: [Channel] -> String
listToStr [] = []
listToStr (x:xs) = x ++ listToStr' xs
    where listToStr' [] = []
          listToStr' [x] = " and " ++ x
          listToStr' (x:xs) = ", " ++ x ++ listToStr' xs

-- annoying
toPretty :: TimeDiff -> String
toPretty td = let secs = tdSec td 
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



