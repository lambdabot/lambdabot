{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Simple wrapper over privmsg to get time information via the CTCP
module Plugin.Localtime (theModule) where

import Plugin
import qualified Data.Map as M
import qualified Message as Msg

$(plugin "Localtime")

type TimeMap = M.Map Msg.Nick  -- the person who's time we requested
                    [Msg.Nick] -- a list of targets waiting on this time

instance Module LocaltimeModule TimeMap where

  moduleHelp _ _      = "time <user>. Print a user's local time. User's client must support ctcp pings."
  moduleCmds   _      = ["time", "localtime", "localtime-reply"]
  moduleDefState _    = return M.empty

  -- synonym
  process x y z "time" a = process x y z "localtime" a

  -- record this person as a callback, for when we (asynchronously) get a result
  process m msg whoAsked "localtime" []     = do
    let n = Msg.nName (Msg.nick msg)
    process m msg whoAsked "localtime" n

  process _ msg whoAsked "localtime" rawWho = do
    let whoToPing = Msg.readNick msg $ fst $ break (== ' ') rawWho
    if whoToPing /= Msg.lambdabotName msg
        then do modifyMS $ \st -> M.insertWith (++) whoToPing [whoAsked] st
            -- this is a CTCP time call, which returns a NOTICE
                lift $ ircPrivmsg' whoToPing ("\^ATIME\^A")     -- has to be raw
                return []
        else return ["I live on the internet, do you expect me to have a local time?"]

  -- the Base module caught the NOTICE TIME, mapped it to a PRIVMGS, and here it is :)
  process _ msg _ "localtime-reply" text = do
    let (whoGotPinged', time') = break (== ':') text
        whoGotPinged = Msg.readNick msg whoGotPinged'
        time = drop 1 time'

    targets <- withMS $ \st set -> do
        case M.lookup whoGotPinged st of
            Nothing -> return []
            Just xs -> do set (M.insert whoGotPinged [] st) -- clear the callback state
                          return xs
    let txt = "Local time for " ++ Msg.showNick msg whoGotPinged ++ " is " ++ time
    lift $ flip mapM_ targets $ flip ircPrivmsg' txt
    return []
