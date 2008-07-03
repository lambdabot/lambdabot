{-# LANGUAGE MultiParamTypeClasses, PatternGuards #-}
-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Haskell evaluator for the pure part, using plugs
module Plugin.Eval where

import Plugin
import Lambdabot.Parser
import Language.Haskell.Parser
import Language.Haskell.Syntax hiding (Module)
import qualified Text.Regex as R
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
                                    x <- io $ comp Nothing
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
    case parseExpr src of
        Left  e -> return e
        Right _ -> do
            (out,err,_) <- popen binary ["+RTS","-M64m"] (Just src)
            case (out,err) of
                ([],[]) -> return "Terminated\n"
                _       -> do
                    let o = munge out
                        e = munge err
                    return $ case () of {_
                        | null o && null e -> "Terminated\n"
                        | null o           -> " " ++ e
                        | otherwise        -> " " ++ o
                    }

------------------------------------------------------------------------
-- define a new binding

define :: String -> IO String
define src = case parseModule (src ++ "\n") of -- extra \n so comments are parsed correctly
    (ParseOk (HsModule _ _ (Just [HsEVar (UnQual (HsIdent "main"))]) [] ds))
        | all okay ds -> comp (Just src)
    (ParseFailed _ e) -> return $ " " ++ e
    _                 -> return "Invalid declaration"
 where
    okay (HsTypeSig   {}) = True
    okay (HsFunBind   {}) = True
    okay (HsPatBind   {}) = True
    okay (HsInfixDecl {}) = True
    okay _                = False

-- It parses. then add it to a temporary L.hs and typecheck
comp :: Maybe String -> IO String
comp src = do
    copyFile "State/L.hs" "L.hs"
    case src of
        Nothing -> return () -- just reset from Pristine
        Just s  -> P.appendFile "L.hs" (P.pack (s  ++ "\n"))

    -- and compile Local.hs
    -- careful with timeouts here. need a wrapper.
    (o',e',c) <- popen "ghc" ["-O","-v0","-c"
                             ,"-Werror"
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
clean_ s|  no_io      `matches'`    s = "No IO allowed\n"
        |  type_sig   `matches'`    s = "Add a type signature\n"
        |  enomem     `matches'`    s = "Tried to use too much memory\n"

        | Just (_,m,_,_) <- ambiguous  `R.matchRegexAll` s = m
        | Just (_,_,b,_) <- inaninst   `R.matchRegexAll` s = clean_ b
        | Just (_,_,b,_) <- irc        `R.matchRegexAll` s = clean_ b
        | Just (_,m,_,_) <- nomatch    `R.matchRegexAll` s = m
        | Just (_,m,_,_) <- notinscope `R.matchRegexAll` s = m
        | Just (_,m,_,_) <- hsplugins  `R.matchRegexAll`  s = m
        | Just (a,_,_,_) <- columnnum  `R.matchRegexAll`  s = a
        | Just (a,_,_,_) <- extraargs  `R.matchRegexAll`  s = a
        | Just (_,_,b,_) <- filename'  `R.matchRegexAll`  s = clean_ b
        | Just (a,_,b,_) <- filename   `R.matchRegexAll`  s = a ++ clean_ b
        | Just (a,_,b,_) <- filepath   `R.matchRegexAll`   s = a ++ clean_ b
        | Just (a,_,b,_) <- runplugs   `R.matchRegexAll`  s = a ++ clean_ b
        | otherwise      = s
    where
        -- s/<[^>]*>:[^:]: //
        type_sig   = regex' "add a type signature that fixes these type"
        no_io      = regex' "No instance for \\(Show \\(IO"
        irc        = regex' "\n*<irc>:[^:]*:[^:]*:\n*"
        filename   = regex' "\n*<[^>]*>:[^:]*:\\?[^:]*:\\?\n* *"
        filename'  = regex' "/tmp/.*\\.hs[^\n]*\n"
        filepath   = regex' "\n*/[^\\.]*.hs:[^:]*:\n* *"
        ambiguous  = regex' "Ambiguous type variable `a\' in the constraints"
        runplugs   = regex' "runplugs: "
        notinscope = regex' "Variable not in scope:[^\n]*"
        hsplugins  = regex' "Compiled, but didn't create object"
        extraargs  = regex' "[ \t\n]*In the [^ ]* argument"
        columnnum  = regex' " at <[^\\.]*\\.[^\\.]*>:[^ ]*"
        nomatch    = regex' "Couldn't match[^\n]*\n"
        inaninst   = regex' "^[ \t]*In a.*$"
        enomem     = regex' "^Heap exhausted"

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
