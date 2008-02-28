{-# LANGUAGE ForeignFunctionInterface #-}

module Resource (setCPULimit) where

import Foreign
import Foreign.C

import qualified Control.Exception

foreign import ccall unsafe "setrlimit"
  c_setrlimit :: CInt -> Ptr () -> IO CInt

-- yay, hard-coded!
setCPULimit :: Word32 -> IO ()
setCPULimit n = do
  allocaBytes 8 $ \p_rlimit -> do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p_rlimit n
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p_rlimit n
    throwErrnoIfMinus1 "setResourceLimit" $
       c_setrlimit (0) p_rlimit -- hard coded zero
    return ()

