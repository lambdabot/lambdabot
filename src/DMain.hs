module DMain where

import LMain
import DynModules
import Lambdabot.Shared

------------------------------------------------------------------------

dynmain :: DynLoad  -> IO ()
dynmain fn = main' (Just fn) modulesInfo
