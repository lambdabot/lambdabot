{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Plugin.Stats (theModule) where

import Plugin
import Lambdabot

import Network.StatsD

host = "stats.thecave.lan"
port = "8125"
prefix = ["lambdabot"]

type Stats = ModuleT StatsD LB

theModule = newModule
    { moduleDefState = io (openStatsD host port prefix)
    , contextual = \_ -> do
        chan <- showNick =<< getTarget
        count ["chat", chan] 1
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
count bucket val = report [stat bucket val "c" Nothing]
