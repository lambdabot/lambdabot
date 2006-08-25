--
-- Copyright (c) 2006-6 Spencer Janssen
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
    moduleCmds   _ = ["undo"]
    moduleHelp _ _ = "undo <expr>\nTranslate do notation to basic Monad operators."
    process_ _ _ args = ios $
        case parseExpr args of
            ParseOk e -> return $ prettyPrint $ transform e
            err       -> return $ show err

transform :: HsExp -> HsExp
transform = everywhere (mkT transDo)

transDo :: HsExp -> HsExp
transDo (HsDo stms) = f stms
 where
    f [HsQualifier e]          = e
    f (HsGenerator s p e : xs) = infixed e ">>=" $ HsLambda s [p] $ f xs
    f (HsQualifier e     : xs) = infixed e ">>" $ f xs
    f (HsLetStmt   ds    : xs) = HsLet ds $ f xs
transDo x           = x

infixed :: HsExp -> String -> HsExp -> HsExp
infixed l o r = HsInfixApp l (HsQVarOp $ UnQual $ HsSymbol o) r
