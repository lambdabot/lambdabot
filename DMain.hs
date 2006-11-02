module DMain where

import LMain
import DynModules
import Shared

------------------------------------------------------------------------

dynmain :: DynLoad  -> IO ()
dynmain fn = main' (Just fn) modulesInfo
