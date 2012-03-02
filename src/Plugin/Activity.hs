{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | Logging an IRC channel..
module Plugin.Activity (theModule) where

import Plugin
import qualified Lambdabot.Message as Msg

import Control.Arrow ((&&&))
import Control.Exception (evaluate)

import System.Time

plugin "Activity"

type ActivityState = [(ClockTime,Msg.Nick)]

helpStr = "activity seconds. Find out where/how much the bot is being used"

instance Module ActivityModule where
    type ModuleState ActivityModule = ActivityState
    moduleDefState _ = return []
    moduleInit _ = bindModule2 activityFilter >>= ircInstallOutputFilter
    
    moduleCmds _ = 
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

activity full args = do
    let obscure nm
            | full || isPrefixOf "#" (Msg.nName nm) = return nm
            | otherwise = readNick "private"
    
    TOD secs ps <- io getClockTime
    let cutoff = TOD (secs - (fromMaybe 90 $ readM args)) ps
    users <- mapM (obscure . snd) . takeWhile ((> cutoff) . fst) =<< lift readMS
    let agg_users = reverse . sort . map (length &&& head) . group . sort $ users
    fmt_agg <- fmap (concatWith " " . (:) (show (length users) ++ "*total"))
                    (mapM (\(n,u) -> do u' <- showNick u; return (show n ++ "*" ++ u')) $ agg_users)
    
    say fmt_agg

activityFilter :: Msg.Nick -> [String] -> Activity [String]
activityFilter target lns = do
    io $ evaluate $ foldr seq () $ map (foldr seq ()) $ lns
    withMS $ \ st wr -> do
        now <- io getClockTime
        wr (map (const (now,target)) lns ++ st)
    return lns
