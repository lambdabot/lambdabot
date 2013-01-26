{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.Stats (theModule) where

import Plugin
import Lambdabot

import Network.Metric

host = "stats.thecave.lan"
port = 8125

type Stats = ModuleT AnySink LB

theModule = newModule
    { moduleDefState = io (open Statsd "statsd" host port)
    , contextual = \_ -> do
        chan <- getTarget
        count "lambdabot.chat" (packNick chan) 1
    }

onJoin :: IrcMessage -> Stats ()
onJoin _ = return ()

-- various helpers

type StatsM m = (MonadIO m, MonadLBState m, Sink (LBState m))

stat :: (Measurable a, StatsM m) => a -> m ()
stat x = do
    st <- readMS
    io (push st x)

metric ctor g b x = stat (ctor g b x)

count :: StatsM m => Group -> Bucket -> Integer -> m ()
count = metric Counter

timer :: StatsM m => Group -> Bucket -> Double -> m ()
timer = metric Timer

gauge :: StatsM m => Group -> Bucket -> Double -> m ()
gauge = metric Gauge
