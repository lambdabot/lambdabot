--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | A Haskell evaluator for the pure part, using plugs
--
-- requires the 'runplugs' utility available with the hs-plugins library.
-- in '$hsplugins/examples/hmake/one-shot'
--
module Plugins.Plugs (theModule) where

import Util             (expandTab)
import Lambdabot      hiding  (clean)
import PosixCompat

import Control.Monad.Trans      ( liftIO )
import Text.Regex

newtype PlugsModule = PlugsModule ()

theModule :: MODULE
theModule = MODULE $ PlugsModule ()

instance Module PlugsModule () where
        moduleHelp _ _ = return "@plugs <expr>\nYou have Haskell, 3 seconds and no IO. Go nuts!"
        moduleCmds   _ = return ["plugs"]
        process _ _ src "plugs" s = do o <- liftIO $ plugs s
                                       ircPrivmsg src o
        process _ _ _ _ _ = error "PlugsModule: invalid command"

binary :: String
binary = "runplugs"

plugs :: String -> IO String
plugs src = do
    (out,err,_) <- popen binary [] (Just src)
    let o = unlines . take 3 . lines . expandTab . clean $ out
        e = unlines . take 3 . lines . expandTab . clean $ err
    return $ case () of {_
        | null o && null e -> "Terminated\n"
        | null o           -> e
        | otherwise        -> o
    }

--
-- Clean up runplugs' output
--
clean :: String -> String
clean s | Just _         <- no_io      `matchRegex`    s = "No IO allowed\n"
        | Just _         <- terminated `matchRegex`    s = "Terminated\n"
        | Just _         <- stack_o_f  `matchRegex`    s = "Stack overflow\n"
        | Just _         <- loop       `matchRegex`    s = "Loop\n"
        | Just _         <- undef      `matchRegex`    s = "Undefined\n"
        | Just (_,m,_,_) <- ambiguous  `matchRegexAll` s = m
        | Just (_,_,b,_) <- irc        `matchRegexAll` s = clean b
        | Just (_,m,_,_) <- nomatch    `matchRegexAll` s = m
        | Just (_,m,_,_) <- notinscope `matchRegexAll` s = m
        | Just (_,m,_,_) <- hsplugins `matchRegexAll`  s = m
        | Just (a,_,_,_) <- columnnum `matchRegexAll`  s = a
        | Just (a,_,_,_) <- extraargs `matchRegexAll`  s = a
        | Just (_,_,b,_) <- filename' `matchRegexAll`  s = clean b
        | Just (a,_,b,_) <- filename  `matchRegexAll`  s = a ++ clean b
        | Just (a,_,b,_) <- filepath `matchRegexAll`   s = a ++ clean b
        | Just (a,_,b,_) <- runplugs  `matchRegexAll`  s = a ++ clean b
        | otherwise      = s
    where
        -- s/<[^>]*>:[^:]: //
        no_io      = mkRegex "No instance for \\(Show \\(IO"
        terminated = mkRegex "waitForProc"
        stack_o_f  = mkRegex "Stack space overflow"
        loop       = mkRegex "runplugs: <<loop>>"
        irc        = mkRegex "\n*<irc>:[^:]*:[^:]*:\n*"
        filename   = mkRegex "\n*<[^>]*>:[^:]*:\\?[^:]*:\\?\n* *"
        filename'  = mkRegex "/tmp/.*\\.hs[^\n]*\n"
        filepath   = mkRegex "\n*/[^\\.]*.hs:[^:]*:\n* *"
        undef      = mkRegex "Prelude.undefined"
        ambiguous  = mkRegex "Ambiguous type variable `a\' in the constraints"
        runplugs   = mkRegex "runplugs: "
        notinscope = mkRegex "Variable not in scope:[^\n]*"
        hsplugins  = mkRegex "Compiled, but didn't create object"
        extraargs  = mkRegex "[ \t\n]*In the [^ ]* argument"
        columnnum  = mkRegex " at <[^\\.]*\\.[^\\.]*>:[^ ]*"
        nomatch    = mkRegex "Couldn't match[^\n]*\n"

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
