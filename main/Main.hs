--
-- | Let's go lambdabot!
--
module Main where

import Lambdabot.Main

import Modules      (modulesInfo)

import System.Posix.Signals

-- do argument handling
main :: IO ()
main = do
    -- when 'popen' is called on a non-existing executable, SIGPIPE is sent,
    -- causing lambdabot to exit:
    _ <- installHandler sigPIPE Ignore Nothing
    lambdabotMain modulesInfo []

-- special online target for ghci use
online :: [String] -> IO ()
online strs = runIrc modulesInfo [onStartupCmds :=> strs]
