{-# OPTIONS -fglasgow-exts #-}
-- ^ For pattern guards
--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- requires the 'runplugs' utility available with the hs-plugins library.
-- in $hsplugins/examples/hmake/one-shot
--

--
-- a Haskell evaluator for the pure part, using `plugs`
--

module PlugsModule where

import IRC      hiding ( clean )
import PosixCompat

import Control.Monad.Trans      ( liftIO )
import Text.Regex

newtype PlugsModule = PlugsModule ()

theModule :: MODULE
theModule = MODULE plugsModule

plugsModule :: PlugsModule
plugsModule = PlugsModule ()

instance Module PlugsModule where
        moduleName   _ = return "plugs"
        moduleHelp _ _ = return "@plugs <expr>\nYou have Haskell, 3 seconds and no IO. Go nuts!"
        moduleSticky _ = False
        commands     _ = return ["plugs"]
        process _ _ src "plugs" s = do o <- liftIO $ plugs s
                                       ircPrivmsg src o
        process _ _ _ _ _ = error "PlugsModule: invalid command"

binary :: String
binary = "runplugs"

plugs :: String -> IO String
plugs src = do
    (out,err,_) <- popen binary [] (Just src)
    let o = clean out
        e = clean err
    return $ case () of {_
        | null o && null e -> "Terminated\n"
        | null o           -> e
        | otherwise        -> o
    }

--
-- Clean up runplugs' output
--
clean :: String -> String
clean s | Just _         <- no_io      `matchRegex`   s = "No IO allowed\n"
        | Just _         <- terminated `matchRegex`   s = "Terminated\n"
        | Just _         <- stack_o_f  `matchRegex`   s = "Stack overflow\n"
        | Just (a,_,b,_) <- filename `matchRegexAll`  s = a ++ clean b
        | Just (a,_,b,_) <- filepath `matchRegexAll`  s = a ++ clean b
        | Just (_,m,_,_) <- hsplugins `matchRegexAll` s = m
        | otherwise      = s
    where
        -- s/<[^>]*>:[^:]: //
        filepath   = mkRegex "\n?/[^\\.]*.hs:[^:]*:\n* *"
        filename   = mkRegex "\n?<[^>]*>:[^:]*:\n* *"
        terminated = mkRegex "waitForProc"
        stack_o_f  = mkRegex "Stack space overflow"
        hsplugins  = mkRegex "Compiled, but didn't create object"
        no_io      = mkRegex "No instance for \\(Show \\(IO"

------------------------------------------------------------------------
--
-- Plugs tests:
--  * too long, should be terminated.
--      @plugs last [ 1 .. 100000000 ]
--      @plugs last [ 1 .. ]
--      @plugs product [1..]
--      @plugs let loop () = loop () in loop () :: ()
--
--  * stack oflow
--      @plugs scanr (*) 1 [1..]
--
--  * type errors, or module scope errors
--      @plugs unsafePerformIO (return 42)
--      @plugs GHC.Exts.I# 1#
--      @plugs $( Language.Haskell.THSyntax.Q (putStr "heya") >> [| 3 |] )
--      @plugs Data.Array.listArray (minBound::Int,maxBound) (repeat 0)
--
--  * syntax errors
--      @plugs map foo bar
--      @plugs $( [| 1 |] )
--
--  * success
--      @plugs head [ 1 .. ]
--      @plugs [1..]
--      @plugs last $ sort [1..100000 ]
--      @plugs let fibs = 1:1:zipWith (+) fibs (tail fibs) in take 20 fibs
--      @plugs sort [1..10000]
--      @plugs ((error "throw me") :: ())
--      @plugs Random.randomRs (0,747737437443734::Integer) (Random.mkStdGen 1122)
--
-- More at http://www.scannedinavian.org/~shae/joyXlogs.txt
--
