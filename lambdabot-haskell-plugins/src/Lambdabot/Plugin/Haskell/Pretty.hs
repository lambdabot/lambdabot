{- | Pretty-Printing echo

example:

> @pretty fun x = case x of {3 -> "hello" ; 5 -> "world" ; _ -> "else"}

> fun x
>  = case x of
>   3 -> "hello"
>   5 -> "world"
>   _ -> "else"

(c) Johannes Ahlmann, 2005-12-13, released under GPL 2 -}

module Lambdabot.Plugin.Haskell.Pretty (prettyPlugin) where

import Lambdabot.Plugin

import Data.List
import qualified Language.Haskell.Exts as Hs
import Language.Haskell.Exts hiding (Module, Pretty)

type Pretty = ModuleT () LB

prettyPlugin :: Module ()
prettyPlugin = newModule
    { moduleCmds = return
        [ (command "pretty")
            { help = say "pretty <expr>. Display haskell code in a pretty-printed manner"
            , process = prettyCmd
            }
        ]
    }

------------------------------------------------------------------------

prettyCmd :: String -> Cmd Pretty ()
prettyCmd rest =
    let code = dropWhile (`elem` " \t>") rest
        modPrefix1 = "module Main where "
        modPrefix2 = "module Main where __expr__ = "
        prefLen1 = length modPrefix1
        result = case (parseModule (modPrefix1 ++ code ++ "\n"), parseModule (modPrefix2 ++ code ++ "\n"))  of
            (ParseOk a, _)            -> doPretty a
            (_, ParseOk a)            -> doPretty a
            (ParseFailed locat msg,_) -> let (SrcLoc _ _ col) = locat in
                   (show msg ++ " at column " ++ show (col - prefLen1)) : []
    in mapM_ say result -- XXX will this work? No, spaces are compressed.

-- | calculates "desired" indentation and return pretty-printed declarations
-- the indentation calculations are still pretty much rough guesswork.
-- i'll have to figure out a way to do some _reliable_ pretty-printing!
doPretty :: Hs.Module -> [String]
doPretty (Hs.Module _ _ _ _ _ _ decls) =
    let defaultLen = 4
        declLen (FunBind mtches)   = maximum $ map matchLen mtches
        declLen (PatBind _ pat _ _) = patLen pat
        declLen _  = defaultLen
        patLen (PVar nm) = nameLen nm
        patLen  _  = defaultLen
        nameLen (Ident s)  = length s + 1
        nameLen _  = defaultLen
        matchLen (Match _ nm pats _ _ _) =
            let l = (nameLen nm + sum (map patLen pats) + 1)
            in if l > 16 then defaultLen else l
        makeMode decl = defaultMode {
            doIndent     = 3,
            caseIndent   = 4,
            onsideIndent = declLen decl
        }
        makeModeExp _ = defaultMode {
            doIndent     = 3,
            caseIndent   = 4,
            onsideIndent = 0
        }
        prettyDecl (PatBind _ (PVar (Ident "__expr__")) (UnGuardedRhs e) Nothing) -- pretty printing an expression
                     = prettyPrintWithMode (makeModeExp e) e
        prettyDecl d = prettyPrintWithMode (makeMode d) d
    -- TODO: prefixing with hashes is done, because i didn't find a way
    --   to disable the indentation filter of lambdabot only for this module...
    in map (" "++) . lines . concat . intersperse "\n"
       -- . map show $ decls
       . map prettyDecl $ decls
