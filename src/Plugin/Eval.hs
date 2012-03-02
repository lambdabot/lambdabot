
{-# LANGUAGE TemplateHaskell, PatternGuards #-}
-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Haskell evaluator for the pure part, using plugs
module Plugin.Eval (theModule, plugs, exts) where

import Lambdabot.File (findFile)
import Plugin
import System.Directory
import System.Exit
import qualified Data.ByteString.Char8 as P
import Control.Exception (try, SomeException)

plugin "Plugs"

instance Module PlugsModule where
    moduleCmds _ =
        [ (command "run")
            { help = say "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
            , process = \s -> do
                to <- getTarget
                ios80 to (plugs s) >>= mapM_ say
            }
        , (command "let")
            { help = say "let <x> = <e>. Add a binding"
            , process = \s -> do
                to <- getTarget
                ios80 to (define s) >>= mapM_ say
            }
        , (command "undefine")
            { help = say "undefine. Reset evaluator local bindings"
            , process = \s -> do
                l <- io $ findFile "L.hs"
                p <- io $ findFile "Pristine.hs"
                io $ copyFile p l
                say "Undefined."
            }
        ]

    contextual _ txt
        | isEval txt = do
            to <- getTarget
            (ios80 to . plugs . dropPrefix $ txt) >>= mapM_ say
        | otherwise  = return ()

binary :: String
binary = "mueval"

-- extensions to enable for the interpreted expression
-- (and probably also L.hs if it doesn't already have these set)
exts :: [String]
exts = []

args :: String -> String -> [String]
args load src = concat
    [ map ("-X" ++) exts
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

-- It parses. then add it to a temporary L.hs and typecheck
define :: String -> IO String
define src = do
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
