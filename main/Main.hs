--
-- | Let's go lambdabot!
--
module Main where

import Lambdabot.Main
import Modules      (modulesInfo)

-- do argument handling
main :: IO ()
main = lambdabotMain modulesInfo []

-- special online target for ghci use
online :: [String] -> IO ()
online strs = runIrc modulesInfo [onStartupCmds :=> strs]
