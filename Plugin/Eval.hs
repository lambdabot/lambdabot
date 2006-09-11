--
-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | A Haskell evaluator for the pure part, using plugs
--
module Plugin.Eval where

import Plugin
import Lib.Parser
import qualified Language.Haskell.Parser as L
import System.Directory
import System.Exit

import qualified Data.ByteString.Char8 as P

PLUGIN Plugs

instance Module PlugsModule () where
    moduleCmds   _             = ["run","let","undefine"]
    moduleHelp _ "let"         = "let <x> = <e>. Add a binding"
    moduleHelp _ "undefine"    = "undefine. Reset evaluator local bindings"
    moduleHelp _ _             = "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
    process _ _ to "run" s     = ios80 to (plugs s)
    process _ _ to "let" s     = ios80 to (define s)
    process _ _ _ "undefine" _ = do io $ copyFile "State/Pristine.hs" "State/L.hs"
                                    x <- io $ compile Nothing
                                    return [x]

    contextual _ _ to txt
        | isEval txt = ios80 to . plugs . dropPrefix $ txt
        | otherwise  = return []

binary :: String
binary = "./runplugs"

isEval :: String -> Bool
isEval = ((evalPrefixes config) `arePrefixesWithSpaceOf`)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

plugs :: String -> IO String
plugs src = do
    -- first, verify the source is actually a Haskell 98 expression, to
    -- avoid code injection bugs.
    case parseExpr (src ++ "\n") of
        ParseFailed _ e -> return $ " " ++ e
        ParseOk     _   -> do
            (out,err,_) <- popen binary [] (Just src)
            let o = munge out
                e = munge err
            return $ case () of {_
                | null o && null e -> "Terminated\n"
                | null o           -> " " ++ e
                | otherwise        -> " " ++ o
            }

------------------------------------------------------------------------
-- define a new binding

-- restrict to value declarations for now
define :: String -> IO String
define src
   | (ParseFailed _ e) <- L.parseModule src        = return $ " " ++ e
   | Nothing           <- isDecl `matchRegex` src  = return "Invalid declaration"
   | (ParseFailed _ e) <- parseExpr (src' ++ "\n") = return $ " " ++ e
   | otherwise                                     = compile (Just src)
   where isDecl = mkRegex "^([a-z][a-zA-Z_'0-9]* *)+= *"
         src'   = drop 1 . dropWhile (/= '=') $ src

-- It parses. then add it to a temporary L.hs and typecheck
compile :: Maybe String -> IO String
compile src = do
    copyFile "State/L.hs" "L.hs"
    case src of
        Nothing -> return () -- just reset from Pristine
        Just s  -> P.appendFile "L.hs" (P.pack (s  ++ "\n"))

    -- and compile Local.hs
    -- careful with timeouts here. need a wrapper.
    (o',e',c) <- popen "ghc" ["-O","-v0","-c"
                             ,"-Wall","-Werror"
                             ,"-fno-warn-incomplete-patterns"
                             ,"-fno-warn-missing-signatures"
                             ,"-fno-warn-overlapping-patterns"
                             ,"-fno-warn-simple-patterns"
                             ,"-fno-warn-type-defaults"
                             ,"-fno-warn-unused-binds"
                             ,"-fno-warn-unused-imports"
                             ,"-fno-warn-unused-matches"
                             ,"-odir", "State/"
                             ,"-hidir","State/"
                             ,"L.hs"] Nothing

    case (munge o', munge e') of
        ([],[]) | c /= ExitSuccess -> return "Error."
                | otherwise -> do
                    renameFile "L.hs" "State/L.hs"
                    renameFile "State/L.hi" "L.hi"
                    renameFile "State/L.o" "L.o"
                    return (maybe "Undefined." (const "Defined.") src)
        (ee,[]) -> return (concat . intersperse " " . lines $ ee)
        (_ ,ee) -> return (concat . intersperse " " . lines $ ee)

-- test cases
-- lambdabot> undefine
-- Undefined.
--
-- lambdabot> let x = 1
-- Defined.
-- lambdabot> let y = L.x
-- Defined.
-- lambdabot> > L.x + L.y
-- 2
-- lambdabot> let type Z = Int
-- Defined.
-- lambdabot> let newtype P = P Int 
-- Defined.
-- lambdabot> > L.P 1 :: L.P
--  add an instance declaration for (Show L.P)
-- lambdabot> let instance Show L.P where show _ = "P"
-- Defined.
-- lambdabot> > L.P 1 :: L.P
--  P
--




munge :: String -> String
munge = expandTab . dropWhile (=='\n') . dropNL . clean_

--
-- Clean up runplugs' output
--
clean_ :: String -> String
clean_ s| Just _         <- no_io      `matchRegex`    s = "No IO allowed\n"
        | Just _         <- terminated `matchRegex`    s = "Terminated\n"
        | Just _         <- hput       `matchRegex`    s = "Terminated\n"
        | Just _         <- stack_o_f  `matchRegex`    s = "Stack overflow\n"
        | Just _         <- loop       `matchRegex`    s = "Loop\n"
        | Just _         <- undef      `matchRegex`    s = "Undefined\n"
        | Just _         <- type_sig   `matchRegex`    s = "Add a type signature\n"
        | Just (_,m,_,_) <- ambiguous  `matchRegexAll` s = m
        | Just (_,_,b,_) <- inaninst   `matchRegexAll` s = clean_ b
        | Just (_,_,b,_) <- irc        `matchRegexAll` s = clean_ b
        | Just (_,m,_,_) <- nomatch    `matchRegexAll` s = m
        | Just (_,m,_,_) <- notinscope `matchRegexAll` s = m
        | Just (_,m,_,_) <- hsplugins `matchRegexAll`  s = m
        | Just (a,_,_,_) <- columnnum `matchRegexAll`  s = a
        | Just (a,_,_,_) <- extraargs `matchRegexAll`  s = a
        | Just (_,_,b,_) <- filename' `matchRegexAll`  s = clean_ b
        | Just (a,_,b,_) <- filename  `matchRegexAll`  s = a ++ clean_ b
        | Just (a,_,b,_) <- filepath `matchRegexAll`   s = a ++ clean_ b
        | Just (a,_,b,_) <- runplugs  `matchRegexAll`  s = a ++ clean_ b
        | otherwise      = s
    where
        -- s/<[^>]*>:[^:]: //
        type_sig   = mkRegex "add a type signature that fixes these type"
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
        inaninst   = mkRegex "^[ \t]*In a.*$"
        hput       = mkRegex "<stdout>: hPutStr"

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
