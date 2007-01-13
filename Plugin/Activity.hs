--
-- | Logging an IRC channel..
--
module Plugin.Activity (theModule) where

import Plugin
import qualified Message as Msg

import Data.Time.Clock
import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

PLUGIN Activity

type ActivityState = [(UTCTime,Msg.Nick)]

instance Module ActivityModule ActivityState where
    moduleHelp _ _              = "activity seconds. Find out where/how much the bot is being used"
    moduleCmds   _              = ["activity"]
    moduleDefState _            = return []
    moduleInit   _              = bindModule2 activityFilter >>=
                                      ircInstallOutputFilter
    process      _ msg _ _ rest = do
        let secs = fromMaybe 90 $ readM rest
        cutoff <- io $ addUTCTime (fromInteger . negate $ secs) `fmap` getCurrentTime
        users <- (map snd . takeWhile ((> cutoff) . fst)) `fmap` readMS
        let agg_users = reverse . sort . map (length &&& head) . group . sort $ users
        let fmt_agg = concatWith " " . (:) (show (length users) ++ "*total") .
                      map (\(n,u) -> show n ++ "*" ++ Msg.showNick msg u) $ agg_users
        return [fmt_agg]

activityFilter :: Msg.Nick -> [String] -> ModuleLB ActivityState
activityFilter target lns = do withMS $ \ st wr -> do
                                 now <- io getCurrentTime
                                 wr (map (const (now,target)) lns ++ st)
                               return lns
