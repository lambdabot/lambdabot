{-# OPTIONS -cpp #-}

module Shell
    ( launch
    , launch' --Interactive IN/OUT/ERR launch of an application.
    , defaultLaunch
    ) where

import Control.Exception
import Control.Monad ( when )
import Foreign
import Foreign.C
import Foreign.C.String
import GHC.Handle
-- import GHC.Posix
import System.Exit
import System.IO
import System.Posix.Types
import System.Posix.Internals

-- import IO

-- import Network.Socket


data Tuple_2_int = Tuple_2_int ()

foreign import ccall get_i1 :: (Ptr Tuple_2_int) -> IO Int
foreign import ccall get_i2 :: (Ptr Tuple_2_int) -> IO Int
foreign import ccall get_i3 :: (Ptr Tuple_2_int) -> IO Int
foreign import ccall get_pid :: (Ptr Tuple_2_int) -> IO CPid
foreign import ccall delete_tuple_2_int :: (Ptr Tuple_2_int) -> IO ()
foreign import ccall "launch" launchC :: CString -> (Ptr CString) -> IO (Ptr Tuple_2_int)
foreign import ccall "wait_for_it" waitForIt :: Ptr Tuple_2_int -> IO Int

defaultLaunch :: FilePath -> [String] -> IO (String, ExitCode)
defaultLaunch = launch f
    where f _ _ output _ = f' output []
	  f' output cs =
	      do outputEOF <- hIsEOF output
		 if outputEOF
		    then return (reverse cs)
		    else do c <- hGetChar output
			    f' output (c:cs)

launch ::  (ProcessID -> Handle -> Handle -> Handle -> IO a)
       -> FilePath -> [String] -> IO (a, ExitCode)
launch f pName pArgs = launch' f pName (pName:pArgs)

launch' :: (ProcessID -> Handle -> Handle -> Handle -> IO a)
        -> FilePath -> [String] -> IO (a, ExitCode)
launch' _ pName [] = error "Shell.pipe needs at least one element in argv."
launch' f pName pArgs = do
  programname <- newCString pName
  arguments <- mapM newCString pArgs
  args <- newArray0 nullPtr arguments --The 0 is for the terminator
  tuple <- launchC programname args
  input  <- get_i1 tuple
  output <- get_i2 tuple
  error <- get_i3 tuple
  pid <- get_pid tuple
#if __GLASGOW_HASKELL__ < 603
  inputH  <- openFd (fromIntegral input)  (Nothing)          "test" WriteMode True False
  outputH <- openFd (fromIntegral output) (Just RegularFile) "test" ReadMode  True False
  errorH  <- openFd (fromIntegral error)  (Just RegularFile) "test" ReadMode  True False
#else
  inputH  <- openFd (fromIntegral input)  (Nothing)          False "test" WriteMode True
  outputH <- openFd (fromIntegral output) (Just RegularFile) False "test" ReadMode  True
  errorH  <- openFd (fromIntegral error)  (Just RegularFile) False "test" ReadMode  True
#endif
  x <- f pid inputH outputH errorH
  bracket 
    (throwErrnoIfMinus1 "system" (waitForIt tuple))
    (\ _ -> do mapM free arguments
               delete_tuple_2_int tuple
    )
    (\ status -> case status of
        0  -> return (x, ExitSuccess)
        n  -> return (x, ExitFailure n)
    )
