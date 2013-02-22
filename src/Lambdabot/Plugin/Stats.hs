{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Lambdabot.Plugin.Stats (theModule) where

import Lambdabot.Plugin
import Lambdabot

import Network.StatsD

host = "stats.thecave.lan"
port = "8125"
prefix = ["lambdabot"]

type Stats = ModuleT StatsD LB

theModule = newModule
    { moduleDefState = io (openStatsD host port prefix)
    , contextual = \msg -> do
        let n = length msg
        
        user <- showNick =<< getSender
        chan <- showNick =<< getTarget
        
        counts 
            [ (grp ++ [stat], val)
            | grp <- [["user", user], ["channel", chan]]
            , (stat, val) <- [("lines", 1), ("chars", toInteger n) ]
            ]
    }

onJoin :: IrcMessage -> Stats ()
onJoin _ = return ()

-- various helpers

type StatsM m = (MonadIO m, MonadLBState m, LBState m ~ StatsD)

report :: StatsM m => [Stat] -> m ()
report xs = do
    st <- readMS
    io (push st xs)

count :: StatsM m => [String] -> Integer -> m ()
count bucket val = counts [(bucket, val)]

counts :: StatsM m => [([String], Integer)] -> m ()
counts xs = report [stat bucket val "c" Nothing | (bucket, val) <- xs]