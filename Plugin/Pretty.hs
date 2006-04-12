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
module Plugin.Pretty where

import Plugin

import Language.Haskell.Parser
import Language.Haskell.Syntax hiding (Module)
import Language.Haskell.Pretty

PLUGIN Pretty

instance Module PrettyModule (String -> IO String) where
    moduleCmds _   = ["pretty"]
    moduleHelp _ _ = "pretty <expr>. Display haskell code in a pretty-printed manner"
    process_ _ _ r = prettyCmd r

------------------------------------------------------------------------

prettyCmd :: String -> ModuleLB (String -> IO String)
prettyCmd rest = 
    let code = dropWhile (`elem` " \t>") rest
        modPrefix = "module Main where "
            ++ if "let" `isPrefixOf` code then "i = " else ""
        prefLen = length modPrefix
        result = case parseModule (modPrefix ++ code) of
            (ParseOk a)           -> doPretty a
            (ParseFailed loc msg) -> let (SrcLoc _ _ col) = loc in
                (show msg ++ " at column " ++ show (col - prefLen)) : []
    in return result -- XXX will this work? No, spaces are compressed.

-- | calculates "desired" indentation and return pretty-printed declarations
-- the indentation calculations are still pretty much rough guesswork.
-- i'll have to figure out a way to do some _reliable_ pretty-printing!
doPretty :: HsModule -> [String]
doPretty (HsModule _ _ _ _ decls) =
    let defaultLen = 4
        declLen (HsFunBind matches)   = maximum $ map matchLen matches
        declLen (HsPatBind _ pat _ _) = patLen pat
        declLen _  = defaultLen
        patLen (HsPVar nm) = nameLen nm
        patLen  _  = defaultLen
        nameLen (HsIdent s)  = length s + 1
        nameLen _  = defaultLen
        matchLen (HsMatch _ nm pats _ _) =
            let l = (nameLen nm + sum (map patLen pats) + 1)
            in if l > 16 then defaultLen else l
        makeMode decl = defaultMode {
            doIndent     = 3,
            caseIndent   = 4,
            onsideIndent = declLen decl
        }
    -- FIXME: prefixing with hashes is done, because i didn't find a way
    --   to disable the indentation filter of lambdabot only for this module...
    in map (" "++) . lines . concat . intersperse "\n" 
       -- . map show $ decls
       . map (\d -> prettyPrintWithMode (makeMode d) d) $ decls
