-- | Logging an IRC channel..
module Lambdabot.Plugin.Activity (theModule) where

import Lambdabot.Plugin
import Lambdabot

import Control.Arrow ((&&&))
import Control.Exception (evaluate)
import Data.List
import Data.Maybe
import Data.Time

type ActivityState = [(UTCTime,Nick)]
type Activity       = ModuleT ActivityState LB

theModule :: Module [(UTCTime, Nick)]
theModule = newModule
    { moduleDefState = return []
    , moduleInit = bindModule2 activityFilter >>= ircInstallOutputFilter

    , moduleCmds = return
        [ (command "activity")
            { help = say helpStr
            , process = activity False
            }
        , (command "activity-full")
            { help = say helpStr
            , privileged = True
            , process = activity True
            }
        ]
    }

helpStr :: String
helpStr = "activity seconds. Find out where/how much the bot is being used"

activity :: Bool -> String -> Cmd Activity ()
activity full args = do
    let obscure nm
            | full || isPrefixOf "#" (nName nm) = return nm
            | otherwise = readNick "private"

    now <- io getCurrentTime
    let cutoff = addUTCTime (- fromInteger (fromMaybe 90 $ readM args)) now
    users <- mapM (obscure . snd) . takeWhile ((> cutoff) . fst) =<< readMS
    let agg_users = reverse . sort . map (length &&& head) . group . sort $ users
    fmt_agg <- fmap (intercalate " " . (:) (show (length users) ++ "*total"))
                    (mapM (\(n,u) -> do u' <- showNick u; return (show n ++ "*" ++ u')) $ agg_users)

    say fmt_agg

activityFilter :: Nick -> [String] -> Activity [String]
activityFilter target lns = do
    io $ evaluate $ foldr seq () $ map (foldr seq ()) $ lns
    withMS $ \ st wr -> do
        now <- io getCurrentTime
        wr (map (const (now,target)) lns ++ st)
    return lns
