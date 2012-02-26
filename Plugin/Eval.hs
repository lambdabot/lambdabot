
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards #-}
-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Haskell evaluator for the pure part, using plugs
module Plugin.Eval where

import File (findFile)
import Plugin
import Lambdabot.Parser
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Text.Regex as R
import System.Directory
import System.Exit
import Codec.Binary.UTF8.String (decodeString)
import qualified Data.ByteString.Char8 as P
import Control.Exception (try, SomeException)

$(plugin "Plugs")

instance Module PlugsModule () where
    moduleCmds   _             = ["run","let","undefine"]
    moduleHelp _ "let"         = "let <x> = <e>. Add a binding"
    moduleHelp _ "undefine"    = "undefine. Reset evaluator local bindings"
    moduleHelp _ _             = "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
    process _ _ to "run" s     = ios80 to (plugs s)
    process _ _ to "let" s     = ios80 to (define s)
    process _ _ _ "undefine" _ = do l <- io $ findFile "L.hs"
                                    p <- io $ findFile "Pristine.hs"
                                    io $ copyFile p l
                                    return ["Undefined."]

    contextual _ _ to txt
        | isEval txt = ios80 to . plugs . dropPrefix $ txt
        | otherwise  = return []

binary :: String
binary = "mueval"

exts 
    | evalUsesSafeHaskell config = 
        [] -- if using safe haskell, L.hs can turn on its own exts
    | otherwise = 
        [ "BangPatterns"
        , "NoMonomorphismRestriction"
        , "MultiParamTypeClasses"
        , "ViewPatterns"
        ]

args :: String -> String -> [String]
args load src = concat
    [ ["-E" | not (evalUsesSafeHaskell config)]
    , map ("-X" ++) exts
    , ["--no-imports", "-l", load]
    , ["--expression=" ++ src]
    , ["+RTS", "-N2", "-RTS"]
    ]

isEval :: String -> Bool
isEval = ((evalPrefixes config) `arePrefixesWithSpaceOf`)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

plugs :: String -> IO String
plugs src = do
            load <- findFile "L.hs"
            (out,err,_) <- popen binary (args load src) Nothing
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
define src
    | evalUsesSafeHaskell config = comp src
    | otherwise = case parseModule (decodeString src ++ "\n") of -- extra \n so comments are parsed correctly
        (ParseOk (Hs.Module _ _ _ _ (Just [Hs.EVar (Hs.UnQual (Hs.Ident "main"))]) [] ds))
            | all okay ds -> comp src
        (ParseFailed _ e) -> return $ " " ++ e
        _                 -> return "Invalid declaration"
 where
    okay (Hs.TypeSig      {}) = True
    okay (Hs.FunBind      {}) = True
    okay (Hs.PatBind      {}) = True
    okay (Hs.InfixDecl    {}) = True
    okay (Hs.TypeDecl     {}) = True
    okay (Hs.DataDecl     {}) = True
    okay (Hs.ClassDecl    {}) = True
    okay (Hs.InstDecl     _ _ hsQName _ _) 
        = case hsQName of
                Hs.Qual _ (Hs.Ident "Typeable")     -> False
                Hs.UnQual (Hs.Ident "Typeable")     -> False
                Hs.Qual _ (Hs.Ident "Data")         -> False
                Hs.UnQual (Hs.Ident "Data")         -> False
                Hs.Qual _ (Hs.Ident "Ix")           -> False
                Hs.UnQual (Hs.Ident "Ix")           -> False
                _                               -> True
    okay _                   = False

-- It parses. then add it to a temporary L.hs and typecheck
comp :: String -> IO String
comp src = do
    l <- findFile "L.hs"
    -- Note we copy to .L.hs, not L.hs. This hides the temporary files as dot-files
    copyFile l ".L.hs"
    P.appendFile ".L.hs" (P.pack (src ++ "\n"))
    
    -- and compile .L.hs
    -- careful with timeouts here. need a wrapper.
    (o',e',c) <- popen "ghc" ["-O","-v0","-c"
                             ,"-Werror"
                             ,".L.hs"] Nothing
    -- cleanup, 'try' because in case of error the files are not generated
    try (removeFile ".L.hi") :: IO (Either SomeException ())
    try (removeFile ".L.o")  :: IO (Either SomeException ())

    case (munge o', munge e') of
        ([],[]) | c /= ExitSuccess -> return "Error."
                | otherwise -> do
                    renameFile ".L.hs" l
                    return "Defined."
        (ee,[]) -> return ee
        (_ ,ee) -> return ee


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
munge = expandTab . dropWhile (=='\n') . dropNL

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
