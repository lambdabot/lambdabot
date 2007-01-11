--
-- | Logging an IRC channel..
--
module Plugin.Activity (theModule) where

import Plugin
import qualified Message as Msg

import System.Time  
import Control.Arrow ((&&&))

PLUGIN Activity

type ActivityState = [(ClockTime,Msg.Nick)]

instance Module ActivityModule ActivityState where
    moduleHelp _ _              = "activity seconds. Find out where/how much the bot is being used"
    moduleCmds   _              = ["activity"]
    moduleDefState _            = return []
    moduleInit   _              = bindModule2 activityFilter >>=
                                      ircInstallOutputFilter
    process      _ msg _ _ rest = do
        secs <- (normalizeTimeDiff . \sec -> TimeDiff 0 0 0 0 0 sec 0) `fmap` readM rest
        now <- io getClockTime
        users <- (map snd . filter ((< secs) . diffClockTimes now . fst)) `fmap` readMS
        let agg_users = sort . map (length &&& head) . group . sort $ users
        let fmt_agg = concatWith " " . (:) (show (length users) ++ "*total") .
                      map (\(n,u) -> show n ++ "*" ++ Msg.showNick msg u) $ agg_users
        return [fmt_agg]

activityFilter :: Msg.Nick -> [String] -> ModuleLB ActivityState
activityFilter target lns = do withMS $ \ st wr -> do
                                 now <- io getClockTime
                                 wr . (++st) . map (const (now,target)) $ lns
                               return lns
