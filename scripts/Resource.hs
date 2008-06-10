{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Resource (ResourceLimit(..), ResourceLimits(..), Resource(..), setResourceLimit) where

#ifndef USE_RESOURCE_WORKAROUND
import System.Posix.Resource
#else

import Foreign
import Foreign.C

import qualified Control.Exception

-- A handy wrapper that provides the same interface as the official library
data ResourceLimit = ResourceLimit Integer

data ResourceLimits = ResourceLimits {
  softLimit, hardLimit :: ResourceLimit
}

data Resource = ResourceCPUTime

setResourceLimit :: Resource -> ResourceLimits -> IO ()
setResourceLimit ResourceCPUTime (ResourceLimits (ResourceLimit soft) (ResourceLimit hard)) =
  setCPULimit (fromInteger soft) (fromInteger hard)

foreign import ccall unsafe "setrlimit"
  c_setrlimit :: CInt -> Ptr () -> IO CInt

-- yay, hard-coded!
setCPULimit :: Word32 -> Word32 -> IO ()
setCPULimit soft hard = do
  allocaBytes 8 $ \p_rlimit -> do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_rlimit soft
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p_rlimit hard
    throwErrnoIfMinus1 "setResourceLimit" $
       c_setrlimit (0) p_rlimit -- hard coded zero
    return ()

#endif

