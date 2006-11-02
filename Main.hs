--
-- | Let's go lambdabot!
--
module Main where

import LMain
import Modules

import Data.Maybe

------------------------------------------------------------------------

(loadStaticModules, _) = modulesInfo

-- do argument handling
main :: IO ()
main = main' Nothing modulesInfo

--
-- special online target for ghci use
online :: IO ()
online = runIrc Online loadStaticModules onlineMain ld pl
    where
    ld = fromMaybe (error "no dynamic loading") Nothing
    pl = []
