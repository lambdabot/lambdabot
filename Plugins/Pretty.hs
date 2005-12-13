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
import Language.Haskell.Syntax (SrcLoc(..), HsModule(..))
import Language.Haskell.Pretty (prettyPrintWithMode, defaultMode)
  -- PPHsMode(..)), PPLayout(..))
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

doPretty :: HsModule -> String
doPretty (HsModule _ _ _ _ decls) =
    let ppMode = defaultMode {-
        doIndent      = 2,
        caseIndent    = 2,
        letIndent     = 2,
        whereIndent   = 2,
        onsideIndent  = 2,
    -}
    in concat . intersperse "\n" . map (prettyPrintWithMode ppMode) $ decls


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
