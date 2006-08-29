--
-- Copyright (c) 2006 Spencer Janssen
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Plugin.Undo where

import Plugin
import Lib.Parser
import Language.Haskell.Syntax hiding (Module)
import Language.Haskell.Pretty
import Data.Generics

PLUGIN Undo

instance Module UndoModule () where
    moduleCmds   _ = ["undo", "redo"]
    moduleHelp _ "undo" = "undo <expr>\nTranslate do notation to Monad operators."
    moduleHelp _ "redo" = "redo <expr>\nTranslate Monad operators to do notation."
    process_ _ cmd args = ios $ return $ transform f args
     where f = case cmd of
                "undo" -> undo
                "redo" -> redo
                _      -> error "unknown command"

ppMode :: PPHsMode
ppMode = defaultMode { layout = PPInLine }

transform :: (HsExp -> HsExp) -> String -> String
transform f s =
    case parseExpr s of
        ParseOk e -> prettyPrintWithMode ppMode $ everywhere (mkT f) e
        err       -> show err

undo :: HsExp -> HsExp
undo (HsDo stms) = f stms
 where
    f [HsQualifier e]          = e
    f (HsGenerator s p e : xs) = infixed e ">>=" $ HsLambda s [p] $ f xs
    f (HsQualifier e     : xs) = infixed e ">>" $ f xs
    f (HsLetStmt   ds    : xs) = HsLet ds $ f xs
undo x           = x

infixed :: HsExp -> String -> HsExp -> HsExp
infixed l o r = HsInfixApp l (HsQVarOp $ UnQual $ HsSymbol o) r

redo :: HsExp -> HsExp
redo (HsLet ds (HsDo s)) = HsDo (HsLetStmt ds : s)
redo app@(HsInfixApp l (HsQVarOp (UnQual (HsSymbol op))) r) = 
    case op of
        ">>=" ->
            case r of
                (HsLambda loc [p] (HsDo stms)) -> HsDo (HsGenerator loc p l : stms)
                (HsLambda loc [p] s)           -> HsDo [HsGenerator loc p l, HsQualifier s]
                _                              -> r
        ">>" ->
            case r of
                (HsDo stms) -> HsDo (HsQualifier l : stms)
                _           -> HsDo [HsQualifier l, HsQualifier r]
        _    -> app
redo x = x
