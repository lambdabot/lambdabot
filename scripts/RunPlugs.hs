--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Runplugs: use hs-plugins to run a Haskell expression under
-- controlled conditions.
--
import System.Eval.Haskell      (unsafeEval_)

import Data.Char                (chr)
import Data.Maybe               (isJust, fromJust)
import Control.Monad
import Control.Exception
import Control.Concurrent      (myThreadId)

import Data.List

import System.Random
import System.Exit              (exitWith, ExitCode(ExitSuccess))
import System.IO                (getContents, hSetBuffering, BufferMode(NoBuffering),
                                 stdout)
import System.Posix.Signals     (sigXCPU, installHandler, Handler(CatchOnce))

import System.Posix.Resource

-- These should not be identical, to give the XCPU handler time to trigger
cpuTimeLimitHard = ResourceLimit 6
cpuTimeLimitSoft = ResourceLimit 5

main = do
    mainThread <- myThreadId
    installHandler sigXCPU (CatchOnce $ throwTo mainThread $ ErrorCall "Time limit exceeded") Nothing
    setResourceLimit ResourceCPUTime $ ResourceLimits cpuTimeLimitSoft cpuTimeLimitHard
    hSetBuffering stdout NoBuffering
    s <- getLine
    context <- fmap ((["L","ShowFun"]++) 
                     . map (unwords . drop 1 . words) 
		     . filter (isPrefixOf "import")
		     . lines) 
		    (readFile "imports.h")
    when (not . null $ s) $ do
        x <- sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
        s <- unsafeEval_ ("let { "++x++
                         " = \n# 1 \"<irc>\"\n"++s++
                         "\n} in P.take 2048 (P.show "++x++
                         ")") context ["-O", "-fasm","-fextended-default-rules","-XNoMonomorphismRestriction","State/L.hs"] [] []
        case s of
            Left  e -> mapM_ putStrLn e
            Right v -> Control.Exception.catch
                (mapM_ putChar v)
                (\e -> Control.Exception.handle (const $ putStrLn "Exception") $ do
                            e' <- Control.Exception.evaluate e
                            putStrLn $ "Exception: " ++ take 1024 (show e'))
    exitWith ExitSuccess

