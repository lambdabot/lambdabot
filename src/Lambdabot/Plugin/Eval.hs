-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Haskell evaluator for the pure part, using mueval
module Lambdabot.Plugin.Eval (theModule, eval, exts) where

import Lambdabot.Plugin
import Lambdabot.Plugin.Base (evalPrefixes)
import Lambdabot.Util.Process

import Control.Exception (try, SomeException)
import Control.Monad
import Data.List
import Data.Ord
import qualified Language.Haskell.Exts as Hs
import System.Directory
import System.Exit

theModule = newModule
    { moduleCmds = return
        [ (command "run")
            { help = say "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
            , process = lim80 . eval
            }
        , (command "let")
            { aliases = ["define"] -- because @define always gets "corrected" to @undefine
            , help = say "let <x> = <e>. Add a binding"
            , process = lim80 . define
            }
        , (command "undefine")
            { help = say "undefine. Reset evaluator local bindings"
            , process = \s -> 
                if null s
                    then do
                        reset
                        say "Undefined."
                    else say "There's currently no way to undefine just one thing.  Say @undefine (with no extra words) to undefine everything."
            }
        ]

    , contextual = \txt -> do
        b <- isEval txt
        when b (lim80 (eval (dropPrefix txt)))
    }

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

isEval :: MonadLB m => String -> m Bool
isEval str = do
    prefixes <- readConfig evalPrefixes
    return (prefixes `arePrefixesWithSpaceOf` str)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

eval :: MonadLB m => String -> m String
eval src = do
    load <- lb (findLBFile "L.hs")
    (out,err,_) <- io (popen binary (args load src) Nothing)
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

define :: MonadLB m => String -> m String
define [] = return "Define what?"
define src = case Hs.parseModule src of
    Hs.ParseOk srcModule -> do
        l <- lb (findLBFile "L.hs")
        res <- io (Hs.parseFile l)
        case res of
            Hs.ParseFailed loc err -> return (Hs.prettyPrint loc ++ ':' : err)
            Hs.ParseOk lModule -> do
                let merged = mergeModules lModule srcModule
                case moduleProblems merged of
                    Just msg -> return msg
                    Nothing  -> comp merged
    Hs.ParseFailed loc err -> return ("Parse failed: " ++ err)

-- merge the second module _into_ the first - meaning where merging doesn't 
-- make sense, the field from the first will be used
mergeModules (Hs.Module loc1 name1 pragmas1 warnings1 exports1 imports1 decls1)
             (Hs.Module    _     _        _         _ exports2 imports2 decls2)
    = Hs.Module loc1 name1 pragmas1 warnings1 exports1
        (mergeImports imports1 imports2)
        (mergeDecls   decls1   decls2)
    where
        mergeImports x y = nub (sortBy (comparing Hs.importModule) (x ++ y))
        mergeDecls x y = sortBy (comparing funcNamesBound) (x ++ y)
        
        -- this is a very conservative measure... we really only even care about function names,
        -- because we just want to sort those together so clauses can be added in the right places
        -- TODO: find out whether the [Hs.Match] can contain clauses for more than one function (e,g. might it be a whole binding group?)
        funcNamesBound (Hs.FunBind ms) = nub $ sort [ n | Hs.Match _ n _ _ _ _ <- ms]
        funcNamesBound _ = []

moduleProblems (Hs.Module _ _ pragmas _ _ imports decls)
    | safe `notElem` langs  = Just "Module has no \"Safe\" language pragma"
    | trusted `elem` langs  = Just "\"Trusted\" language pragma is set"
    | otherwise             = Nothing
    where
        safe    = Hs.name "Safe"
        trusted = Hs.name "Trusted"
        langs = concat [ ls | Hs.LanguagePragma _ ls <- pragmas ]

-- It parses. then add it to a temporary L.hs and typecheck
comp :: MonadLB m => Hs.Module -> m String
comp src = do
    -- Note we copy to .L.hs, not L.hs. This hides the temporary files as dot-files
    io (writeFile ".L.hs" (Hs.prettyPrint src))
    
    -- and compile .L.hs
    -- careful with timeouts here. need a wrapper.
    (o',e',c) <- io (popen "ghc" ["-O","-v0","-c"
                                 ,"-Werror"
                                 ,".L.hs"] Nothing)
    -- cleanup, 'try' because in case of error the files are not generated
    io (try (removeFile ".L.hi") :: IO (Either SomeException ()))
    io (try (removeFile ".L.o")  :: IO (Either SomeException ()))

    case (munge o', munge e') of
        ([],[]) | c /= ExitSuccess -> do
                    io (removeFile ".L.hs")
                    return "Error."
                | otherwise -> do
                    l <- lb (findLBFile "L.hs")
                    io (renameFile ".L.hs" l)
                    return "Defined."
        (ee,[]) -> return ee
        (_ ,ee) -> return ee

munge :: String -> String
munge = expandTab 8 . dropWhile (=='\n') . dropNL

------------------------------
-- reset all bindings

reset :: MonadLB m => m ()
reset = do
    l <- lb (findLBFile "L.hs")
    p <- lb (findLBFile "Pristine.hs")
    io (copyFile p l)
