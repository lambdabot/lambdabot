--
-- | Pretty-Printing echo
--
-- example:
--
-- @pretty fun x = case x of {3 -> "hello" ; 5 -> "world" ; _ -> "else"}
--
-- fun x
--  = case x of
--    3 -> "hello"
--    5 -> "world"
--    _ -> "else"
--
-- (c) Johannes Ahlmann, 2005-12-13, released under GPL 2
--
module Plugins.Pretty where

import Lambdabot
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Language.Haskell.Syntax 
    (HsMatch(..), HsDecl(..), HsName(..), HsPat(..), SrcLoc(..), HsModule(..))
import Language.Haskell.Pretty 
    (prettyPrintWithMode, defaultMode, PPHsMode(..))
import List (intersperse, isPrefixOf)

------------------------------------------------------------------------

newtype PrettyModule = PrettyModule ()

theModule :: MODULE
theModule = MODULE $ PrettyModule ()

instance Module PrettyModule (String -> IO String) where
    moduleHelp _ _   = return $ "echo haskell code in a pretty-printed manner"
    moduleCmds     _ = return ["pretty"]
    process _ _ src cmd rest = case cmd of
        "pretty" -> prettyCmd src rest
        _        -> error "unknown command"

-- | calculates "desired" indentation and return pretty-printed declarations
-- the indentation calculations are still pretty much rough guesswork.
-- i'll have to figure out a way to do some _reliable_ pretty-printing!
doPretty :: HsModule -> String
doPretty (HsModule _ _ _ _ decls) =
    let defaultLen = 4
        declLen (HsFunBind matches)   = maximum $ map matchLen matches
        declLen (HsPatBind _ pat _ _) = patLen pat
        declLen _  = defaultLen
        patLen (HsPVar name) = nameLen name
        patLen  _  = defaultLen
        nameLen (HsIdent s)  = length s + 1
        nameLen _  = defaultLen
        matchLen (HsMatch _ name pats _ _) = 
            let l = (nameLen name + sum (map patLen pats) + 1)
            in if l > 16 then defaultLen else l
        makeMode decl = defaultMode {
            doIndent     = 3,
            caseIndent   = 4,
            onsideIndent = declLen decl
        }
    -- FIXME: prefixing with hashes is done, because i didn't find a way
    --   to disable the indentation filter of lambdabot only for this module...
    in unlines . map ("#  "++) . lines . concat . intersperse "\n" 
       -- . map show $ decls
       . map (\d -> prettyPrintWithMode (makeMode d) d) $ decls


-- FIXME: the solution of prefixing code with a module prelude
--   is pretty ugly, but for the moment it works well enough!
prettyCmd :: String -> String -> ModuleT (String -> IO String) LB ()
prettyCmd src rest = 
    let code = dropWhile (`elem` " \t>") rest
        modPrefix = "module Main where " 
            ++ if "let" `isPrefixOf` code then "i = " else ""
        prefLen = length modPrefix
        result = case parseModule (modPrefix ++ code) of
            (ParseOk a)           -> doPretty a
            (ParseFailed loc msg) -> let (SrcLoc _ _ col) = loc in
                show msg ++ " at column " ++ show (col - prefLen)
    in ircPrivmsg src result

------------------------------------------------------------------------
