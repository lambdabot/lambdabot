--
-- | Let's go lambdabot!
--
module Main where

import Lambdabot
import Lambdabot.Main

import Modules      (modulesInfo)

import Data.Maybe
import System.Posix.Signals

-- do argument handling
main :: IO ()
main = do
    -- when 'popen' is called on a non-existing executable, SIGPIPE is sent,
    -- causing lambdabot to exit:
    _ <- installHandler sigPIPE Ignore Nothing
    main' defaultConfig Nothing modulesInfo

-- special online target for ghci use
online :: [String] -> IO ()
online strs = runIrc defaultConfig strs (fst modulesInfo) ld pl
    where
    ld = fromMaybe (error "no dynamic loading") Nothing
    pl = []
