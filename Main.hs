--
-- | Let's go lambdabot!
--
module Main where

import LMain
import Modules      (modulesInfo)
import Lambdabot

import Data.Maybe

-- do argument handling
main :: IO ()
main = main' Nothing modulesInfo

-- special online target for ghci use
online :: IO ()
online = runIrc [] (fst modulesInfo) onlineMain ld pl
    where
    ld = fromMaybe (error "no dynamic loading") Nothing
    pl = []
