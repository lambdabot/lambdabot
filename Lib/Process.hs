--
-- Copyright (c) 2004-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | A Posix.popen compatibility mapping.
--
module Lib.Process (popen, run) where

import System.Exit
import System.IO
import System.Process
import Control.Concurrent       (forkIO, newEmptyMVar, putMVar, takeMVar)

import qualified Control.Exception

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

--
-- Ignoring exit status for now.
--
-- You have to ignore SIGPIPE, otherwise popening a non-existing executable
-- will result in an attempt to write to a closed pipe and crash the wholw
-- program.
--
-- XXX there are still issues. Large amounts of output can cause what
-- seems to be a dead lock on the pipe write from runplugs, for example.
-- Posix.popen doesn't have this problem, so maybe we can reproduce its
-- pipe handling somehow.
--
popen :: FilePath -> [String] -> Maybe String -> IO (String,String,ExitCode)
popen file args minput =
    Control.Exception.handle (\e -> return ([],show e,error (show e))) $ do

    (inp,out,err,pid) <- runInteractiveProcess file args Nothing Nothing

    case minput of
        Just input -> hPutStr inp input >> hClose inp -- importante!
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

    forkIO (Control.Exception.evaluate (length output) >> putMVar outMVar ())
    forkIO (Control.Exception.evaluate (length errput) >> putMVar errMVar ())

    takeMVar outMVar
    takeMVar errMVar

    -- And now we wait. We must wait after we read, unsurprisingly.
    -- blocks without -threaded, you're warned.
    -- and maybe the process has already completed..
    e <- Control.Exception.catch (waitForProcess pid) (\_ -> return ExitSuccess)

    return (output,errput,e)
