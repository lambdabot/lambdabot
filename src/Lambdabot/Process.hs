-- Copyright (c) 2004-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Posix.popen compatibility mapping.
module Lambdabot.Process (popen, run) where

import System.Exit
import System.IO
import System.Process
import Control.Concurrent       (forkIO, newEmptyMVar, putMVar, takeMVar, killThread)
import Control.Monad
import qualified Control.Exception as E

run :: FilePath -> String -> (String -> String) -> IO String
run binary src scrub = do
    (out,err,_) <- popen binary [] (Just src)
    let o = scrub out
        e = scrub err
    return $ case () of {_
        | null o && null e -> "Done."
        | null o           -> e
        | otherwise        -> o
    }

-- | popen lets you run a binary with specified arguments. This bypasses the shell.
-- | It'll also terminate (SIGTERM) the spawned process in case of
-- | exception, this is very important if the timeout for a Plugin
-- | expires while it is waiting for the result of a looping process.
-- | It's fundamental to link the final executable with -threaded.
popen :: FilePath -- ^ The binary to execute
      -> [String] -- ^ A list of arguments to pass to the binary. No need to
                 -- space separate them
      -> Maybe String -- ^ stdin
      -> IO (String,String,ExitCode)
popen file args minput =
  E.handle (\(E.SomeException e) -> return ([],show e,error (show e))) $ 
   E.bracketOnError (runInteractiveProcess file args Nothing Nothing) (\(_,_,_,pid) -> terminateProcess pid) $
     \(inp,out,err,pid) -> do

    case minput of
        Just input -> hPutStr inp input >> E.catch (hClose inp)
                                                   (\(E.SomeException e) -> return ())
        Nothing    -> return ()

    -- Now, grab the input
    output <- hGetContents out
    errput <- hGetContents err

    -- SimonM sez:
    -- ... avoids blocking the main thread, but ensures that all the
    -- data gets pulled as it becomes available. you have to force the
    -- output strings before waiting for the process to terminate.
    --

    -- Samb says:
    -- Might as well try to avoid hanging my system...
    -- make sure it happens FIRST.

    outMVar <- newEmptyMVar
    errMVar <- newEmptyMVar

    E.bracketOnError (do t1 <- forkIO (E.evaluate (length output) >> putMVar outMVar ())
                         t2 <- forkIO (E.evaluate (length errput) >> putMVar errMVar ())
                         return (t1,t2))
                     (\(t1,t2) -> killThread t1    >> killThread t2   )
                     (\_ ->       takeMVar outMVar >> takeMVar errMVar)

    -- And now we wait. We must wait after we read, unsurprisingly.
    -- blocks without -threaded, you're warned.
    -- and maybe the process has already completed..
    e <- waitForProcess pid 
    return (output,errput,e)
