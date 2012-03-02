
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
            , process = ios80 . plugs
            }
        , (command "let")
            { help = say "let <x> = <e>. Add a binding"
            , process = ios80 . define
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
        | isEval txt = ios80 (plugs (dropPrefix txt))
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
        ([],[]) | c /= ExitSuccess -> do
                    removeFile ".L.hs"
                    return "Error."
                | otherwise -> do
                    renameFile ".L.hs" l
                    return "Defined."
        (ee,[]) -> return ee
        (_ ,ee) -> return ee

munge :: String -> String
munge = expandTab . dropWhile (=='\n') . dropNL
