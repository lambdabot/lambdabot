{-# LANGUAGE CPP #-}

-- Copyright (c) 2004-6 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A Haskell evaluator for the pure part, using mueval
module Lambdabot.Plugin.Haskell.Eval (evalPlugin, runGHC, findL_hs) where

import Lambdabot.Config.Haskell
import Lambdabot.Plugin
import Lambdabot.Util
import Lambdabot.Util.Browser

import Control.Exception (try, SomeException)
import Control.Monad
import Data.List
import Data.Ord
import qualified Language.Haskell.Exts as Hs
import System.Directory
import System.Exit
import System.Process
import Codec.Binary.UTF8.String
import Network.Browser (request)
import Network.HTTP (getRequest, rspBody)

evalPlugin :: Module ()
evalPlugin = newModule
    { moduleCmds = return
        [ (command "run")
            { help = say "run <expr>. You have Haskell, 3 seconds and no IO. Go nuts!"
            , process = lim80 . runGHC
            }
        , (command "let")
            { aliases = ["define"] -- because @define always gets "corrected" to @undefine
            , help = say "let <x> = <e>. Add a binding"
            , process = lim80 . define
            }
        , (command "letlpaste")
            { help = say "letlpaste <paste_id>. Import the contents of an lpaste."
            , process = lim80 . defineFromLPaste
            }
        , (command "undefine")
            { help = say "undefine. Reset evaluator local bindings"
            , process = \s ->
                if null s
                    then do
                        resetL_hs
                        say "Undefined."
                    else say "There's currently no way to undefine just one thing.  Say @undefine (with no extra words) to undefine everything."
            }
        ]

    , contextual = \txt -> do
        b <- isEval txt
        when b (lim80 (runGHC (dropPrefix txt)))
    }

args :: String -> String -> [String] -> [String] -> [String]
args load src exts trusted = concat
    [ ["-S"]
    , map ("-s" ++) trusted
    , map ("-X" ++) exts
    , ["--no-imports", "-l", load]
    , ["--expression=" ++ decodeString src]
    , ["+RTS", "-N", "-RTS"]
    ]

isEval :: MonadLB m => String -> m Bool
isEval str = do
    prefixes <- getConfig evalPrefixes
    return (prefixes `arePrefixesWithSpaceOf` str)

dropPrefix :: String -> String
dropPrefix = dropWhile (' ' ==) . drop 2

runGHC :: MonadLB m => String -> m String
runGHC src = do
    load    <- findL_hs
    binary  <- getConfig muevalBinary
    exts    <- getConfig languageExts
    trusted <- getConfig trustedPackages
    (_,out,err) <- io (readProcessWithExitCode binary (args load src exts trusted) "")
    case (out,err) of
        ([],[]) -> return "Terminated\n"
        _       -> do
            let o = mungeEnc out
                e = mungeEnc err
            return $ case () of {_
                | null o && null e -> "Terminated\n"
                | null o           -> e
                | otherwise        -> o
            }

------------------------------------------------------------------------
-- define a new binding

define :: MonadLB m => String -> m String
define [] = return "Define what?"
define src = do
    exts <- getConfig languageExts
    let mode = Hs.defaultParseMode{ Hs.extensions = map Hs.classifyExtension exts }
    case Hs.parseModuleWithMode mode (decodeString src) of
        Hs.ParseOk srcModule -> do
            l <- findL_hs
            res <- io (Hs.parseFile l)
            case res of
                Hs.ParseFailed loc err -> return (Hs.prettyPrint loc ++ ':' : err)
                Hs.ParseOk lModule -> do
                    let merged = mergeModules lModule srcModule
                    case moduleProblems merged of
                        Just msg -> return msg
                        Nothing  -> comp merged
        Hs.ParseFailed _loc err -> return ("Parse failed: " ++ err)

-- merge the second module _into_ the first - meaning where merging doesn't
-- make sense, the field from the first will be used
mergeModules :: Hs.Module -> Hs.Module -> Hs.Module
mergeModules (Hs.Module loc1 name1 pragmas1 warnings1  exports1 imports1 decls1)
             (Hs.Module    _     _        _         _ _exports2 imports2 decls2)
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

moduleProblems :: Hs.Module -> Maybe [Char]
moduleProblems (Hs.Module _ _ pragmas _ _ _imports _decls)
    | safe `notElem` langs  = Just "Module has no \"Safe\" language pragma"
    | trusted `elem` langs  = Just "\"Trustworthy\" language pragma is set"
    | otherwise             = Nothing
    where
        safe    = Hs.name "Safe"
        trusted = Hs.name "Trustworthy"
        langs = concat [ ls | Hs.LanguagePragma _ ls <- pragmas ]

-- It parses. then add it to a temporary L.hs and typecheck
comp :: MonadLB m => Hs.Module -> m String
comp src = do
    -- Note we copy to .L.hs, not L.hs. This hides the temporary files as dot-files
    io (writeFile ".L.hs" (Hs.prettyPrint src))

    -- and compile .L.hs
    -- careful with timeouts here. need a wrapper.
    trusted <- getConfig trustedPackages
    let ghcArgs = concat
            [ ["-O", "-v0", "-c", "-Werror", "-fpackage-trust"]
            , concat [["-trust", pkg] | pkg <- trusted]
            , [".L.hs"]
            ]
    ghc <- getConfig ghcBinary
    (c, o',e') <- io (readProcessWithExitCode ghc ghcArgs "")
    -- cleanup, 'try' because in case of error the files are not generated
    _ <- io (try (removeFile ".L.hi") :: IO (Either SomeException ()))
    _ <- io (try (removeFile ".L.o")  :: IO (Either SomeException ()))

    case (mungeEnc o', mungeEnc e') of
        ([],[]) | c /= ExitSuccess -> do
                    io (removeFile ".L.hs")
                    return "Error."
                | otherwise -> do
                    l <- lb (findLBFileForWriting "L.hs")
                    io (renameFile ".L.hs" l)
                    return "Defined."
        (ee,[]) -> return ee
        (_ ,ee) -> return ee

munge, mungeEnc :: String -> String
munge = expandTab 8 . strip (=='\n')
mungeEnc = encodeString . munge

------------------------------
-- define from lpaste

defineFromLPaste :: MonadLB m => String -> m String
defineFromLPaste num = do
  maxlen <- getConfig maxPasteLength
  mcode <- fetchLPaste num
  case mcode of
    Left err   -> return err
    Right code
     | length code < maxlen -> define code
     | otherwise            -> return $
       "That paste is too long! (maximum length: " ++ show maxlen ++ ")"

fetchLPaste :: MonadLB m => String -> m (Either String String)
fetchLPaste num = browseLB $
  if any (`notElem` ['0'..'9']) num
    then return $ Left "Invalid paste ID."
    else do
      let src = "http://lpaste.net/raw/" ++ num
      (uri, resp) <- request $ getRequest src
      return $ if show uri == src
        then Right $ rspBody resp
        else Left "I couldn't find any paste under that ID."

------------------------------
-- reset all bindings

resetL_hs :: MonadLB m => m ()
resetL_hs = do
    p <- findPristine_hs
    l <- lb (findLBFileForWriting "L.hs")
    io (copyFile p l)

-- find Pristine.hs; if not found, we try to install a compiler-specific
-- version from lambdabot's data directory, and finally the default one.
findPristine_hs :: MonadLB m => m FilePath
findPristine_hs = do
    p <- lb (findLBFileForReading "Pristine.hs")
    case p of
        Nothing -> do
            p <- lb (findOrCreateLBFile "Pristine.hs")
            p0 <- lb (findLBFileForReading ("Pristine.hs." ++ show __GLASGOW_HASKELL__))
            p0 <- case p0 of
                Nothing -> lb (findLBFileForReading "Pristine.hs.default")
                p0 -> return p0
            case p0 of
                Just p0 -> do
                    p <- lb (findLBFileForWriting "Pristine.hs")
                    io (copyFile p0 p)
                _ -> return ()
            return p
        Just p -> return p

-- find L.hs; if not found, we copy it from Pristine.hs
findL_hs :: MonadLB m => m FilePath
findL_hs = do
    file <- lb (findLBFileForReading "L.hs")
    case file of
        -- if L.hs
        Nothing -> resetL_hs >> lb (findOrCreateLBFile "L.hs")
        Just file -> return file
