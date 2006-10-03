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
import qualified Data.Set as Set
import Control.Monad (guard)

PLUGIN Undo

instance Module UndoModule () where
    moduleCmds   _ = ["undo", "redo"]
    moduleHelp _ "undo" = "undo <expr>\nTranslate do notation to Monad operators."
    moduleHelp _ "redo" = "redo <expr>\nTranslate Monad operators to do notation."
    process_ _ cmd args = ios $ return $ transform f args
     where f = case cmd of
                "undo" -> undo . findVar
                "redo" -> const $ redo
                _      -> error "unknown command"

ppMode :: PPHsMode
ppMode = defaultMode { layout = PPInLine }

findVar :: HsExp -> String
findVar e = head $ do
                    i <- [0 ..]
                    x <- ['a' .. 'z']
                    let xi = x : replicate i '\''
                    guard $ not $ Set.member xi s
                    return xi
 where s = Set.fromList $ listify (const True :: String -> Bool) e

transform :: (HsExp -> HsExp -> HsExp) -> String -> String
transform f s =
    case parseExpr (s ++ "\n") of -- newline to make comments work
        ParseOk e -> prettyPrintWithMode ppMode $ everywhere (mkT $ f e) e
        err       -> show err

undo :: String -> HsExp -> HsExp
undo v (HsDo stms) = f stms
 where
    f [HsQualifier e]          = e
    f (HsQualifier e     : xs) = infixed e ">>" $ f xs
    f (HsLetStmt   ds    : xs) = HsLet ds $ f xs
    f (HsGenerator s p e : xs) 
        | irrefutable p = infixed e ">>=" $ HsLambda s [p] $ f xs
        | otherwise     = infixed e ">>=" $ 
                            HsLambda s [HsPVar $ HsIdent v] $ 
                                HsCase (HsVar $ UnQual $ HsIdent v) 
                                    [ alt p (f xs)
                                    , alt HsPWildCard $
                                        HsApp
                                            (HsVar $ UnQual $ HsIdent "fail")
                                            (HsLit $ HsString "")
                                    ]
        where alt pat x = HsAlt s pat (HsUnGuardedAlt x) []
undo _ x           = x

irrefutable :: HsPat -> Bool
irrefutable (HsPVar _)     = True
irrefutable (HsPIrrPat _)  = True
irrefutable HsPWildCard    = True
irrefutable (HsPAsPat _ p) = irrefutable p
irrefutable (HsPParen p)   = irrefutable p
irrefutable (HsPTuple ps)  = all irrefutable ps
irrefutable _              = False

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
                _                              -> HsDo [HsGenerator undefined HsPWildCard l, HsQualifier r]
        ">>" ->
            case r of
                (HsDo stms) -> HsDo (HsQualifier l : stms)
                _           -> HsDo [HsQualifier l, HsQualifier r]
        _    -> app
redo x = x
