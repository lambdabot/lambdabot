-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Haskell evaluator for the pure part, using mueval
module Plugin.Eval (theModule, eval, exts) where

import Plugin

import Data.Ord
import qualified Language.Haskell.Exts as Hs
import System.Directory
import System.Exit
import Control.Exception (try, SomeException)

theModule = newModule
    { moduleCmds = return
        [ (command "run")
            { help = say "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
            , process = ios80 . eval
            }
        , (command "let")
            { aliases = ["define"] -- because @define always gets "corrected" to @undefine
            , help = say "let <x> = <e>. Add a binding"
            , process = ios80 . define
            }
        , (command "undefine")
            { help = say "undefine. Reset evaluator local bindings"
            , process = \s -> do
                io reset
                say "Undefined."
            }
        ]

    , contextual = \txt ->
        when (isEval txt)
            (ios80 (eval (dropPrefix txt)))
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

isEval :: String -> Bool
isEval = ((evalPrefixes config) `arePrefixesWithSpaceOf`)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

eval :: String -> IO String
eval src = do
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
define [] = return "Define what?"
define src = case Hs.parseModule src of
    Hs.ParseOk srcModule -> do
        l <- findFile "L.hs"
        res <- Hs.parseFile l
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
comp :: Hs.Module -> IO String
comp src = do
    -- Note we copy to .L.hs, not L.hs. This hides the temporary files as dot-files
    writeFile ".L.hs" (Hs.prettyPrint src)
    
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
                    l <- findFile "L.hs"
                    renameFile ".L.hs" l
                    return "Defined."
        (ee,[]) -> return ee
        (_ ,ee) -> return ee

munge :: String -> String
munge = expandTab 8 . dropWhile (=='\n') . dropNL

------------------------------
-- reset all bindings

reset :: IO ()
reset = do
    l <- findFile "L.hs"
    p <- findFile "Pristine.hs"
    copyFile p l
