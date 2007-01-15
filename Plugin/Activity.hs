--
-- | Logging an IRC channel..
--
module Plugin.Activity (theModule) where

import Plugin
import qualified Message as Msg

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Control.Exception (evaluate)

import System.Time

PLUGIN Activity

type ActivityState = [(ClockTime,Msg.Nick)]

instance Module ActivityModule ActivityState where
    moduleHelp _ _              = "activity seconds. Find out where/how much the bot is being used"
    moduleCmds   _              = ["activity"]
    moduleDefState _            = return []
    moduleInit   _              = bindModule2 activityFilter >>=
                                      ircInstallOutputFilter
    process      _ msg _ _ rest = do
        TOD secs ps <- io getClockTime
        let cutoff = TOD (secs - (fromMaybe 90 $ readM rest)) ps
        users <- (map snd . takeWhile ((> cutoff) . fst)) `fmap` readMS
        let agg_users = reverse . sort . map (length &&& head) . group . sort $ users
        let fmt_agg = concatWith " " . (:) (show (length users) ++ "*total") .
                      map (\(n,u) -> show n ++ "*" ++ Msg.showNick msg u) $ agg_users
        return [fmt_agg]

activityFilter :: Msg.Nick -> [String] -> ModuleLB ActivityState
activityFilter target lns = do io $ evaluate $ foldr seq () $ map (foldr seq ()) $ lns
                               withMS $ \ st wr -> do
                                 now <- io getClockTime
                                 wr (map (const (now,target)) lns ++ st)
                               return lns
