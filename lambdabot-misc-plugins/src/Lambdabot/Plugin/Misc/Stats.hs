{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Lambdabot.Plugin.Misc.Stats (statsPlugin) where

import Lambdabot.Plugin
import Lambdabot.Util

import Network.StatsD


type Stats = ModuleT StatsD LB

statsPlugin :: Module StatsD
statsPlugin = newModule
    { moduleDefState = io (openStatsD host port prefix)
    , contextual = \msg -> do
        let n = length msg

        user <- showNick =<< getSender
        chan <- showNick =<< getTarget

        counts
            [ (grp ++ [stat'], val')
            | grp <- [["user", user], ["channel", chan]]
            , (stat', val') <- [("lines", 1), ("chars", toInteger n) ]
            ]
    }

-- various helpers
host :: String
host = "stats.thecave.lan"

port :: String
port = "8125"

prefix :: [String]
prefix = ["lambdabot"]

report :: [Stat] -> Cmd Stats ()
report xs = do
    st <- readMS
    io (push st xs)

counts :: [([String], Integer)] -> Cmd Stats ()
counts xs = report [stat bucket' val' "c" Nothing | (bucket', val') <- xs]
