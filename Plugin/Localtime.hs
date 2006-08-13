--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | Simple wrapper over privmsg to get time information via the CTCP
--
--
module Plugin.Localtime (theModule) where

import Plugin
import qualified Data.Map as M

PLUGIN Localtime

type TimeMap = M.Map String  -- the person who's time we requested
                    [String] -- a list of targets waiting on this time

instance Module LocaltimeModule TimeMap where

  moduleHelp _ _      = "time <user>. Print a user's local time. User's client must support ctcp pings."
  moduleCmds   _      = ["time", "localtime", "localtime-reply"]
  moduleDefState _    = return M.empty

  -- synonym
  process x y z "time" a = process x y z "localtime" a

  -- record this person as a callback, for when we (asynchronously) get a result
  process _ _ whoAsked "localtime" rawWho = do
        let (whoToPing,_) = break (== ' ') rawWho
        modifyMS $ \st -> M.insertWith (++) whoToPing [whoAsked] st
        -- this is a CTCP time call, which returns a NOTICE
        lift $ ircPrivmsg' whoToPing (Just "\^ATIME\^A")     -- has to be raw
        return []

  -- the Base module caught the NOTICE TIME, mapped it to a PRIVMGS, and here it is :)
  process _ _ _ "localtime-reply" text = do
    let (whoGotPinged, time') = break (== ':') text
        time = drop 1 time'

    targets <- withMS $ \st set -> do
        case M.lookup whoGotPinged st of
            Nothing -> return []
            Just xs -> do set (M.insert whoGotPinged [] st) -- clear the callback state
                          return xs
    let msg = "Local time for " ++ whoGotPinged ++ " is " ++ time
    lift $ flip mapM_ targets $ flip ircPrivmsg' (Just msg)
    return []
