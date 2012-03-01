{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Simple wrapper over privmsg to get time information via the CTCP
module Plugin.Localtime (theModule) where

import Plugin
import qualified Data.Map as M
import Lambdabot.Message (Nick, nName)

$(plugin "Localtime")

type TimeMap = M.Map Nick  -- the person who's time we requested
                    [Nick] -- a list of targets waiting on this time

instance Module LocaltimeModule where
  type ModuleState LocaltimeModule = TimeMap
  moduleDefState _    = return M.empty

  moduleCmds _ = 
      [ (command "time")
          { aliases = ["localtime"]
          , help = say "time <user>. Print a user's local time. User's client must support ctcp pings."
          , process = doLocalTime
          }
      , (command "localtime-reply")
          { help = say "time <user>. Print a user's local time. User's client must support ctcp pings."
          , process = doReply
          }
      ]

-- record this person as a callback, for when we (asynchronously) get a result
doLocalTime [] = do
    n <- getSender
    doLocalTime (nName n)

doLocalTime rawWho = do
    whoAsked <- getTarget
    whoToPing <- readNick $ fst $ break (== ' ') rawWho
    me <- getLambdabotName
    if whoToPing /= me
        then lift $ do 
            modifyMS $ \st -> M.insertWith (++) whoToPing [whoAsked] st
            -- this is a CTCP time call, which returns a NOTICE
            lift $ ircPrivmsg' whoToPing ("\^ATIME\^A")     -- has to be raw
        else say "I live on the internet, do you expect me to have a local time?"

-- the Base module caught the NOTICE TIME, mapped it to a PRIVMGS, and here it is :)
doReply text = do
    let (whoGotPinged', time') = break (== ':') text
        time = drop 1 time'
    whoGotPinged <- readNick whoGotPinged'
    
    targets <- lift $ withMS $ \st set -> do
        case M.lookup whoGotPinged st of
            Nothing -> return []
            Just xs -> do set (M.insert whoGotPinged [] st) -- clear the callback state
                          return xs
    whoGotPinged'' <- showNick whoGotPinged
    let txt = "Local time for " ++ whoGotPinged'' ++ " is " ++ time
    lift $ lift $ flip mapM_ targets $ flip ircPrivmsg' txt
