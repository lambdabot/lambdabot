{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

-- Copyright (c) 2006 Spencer Janssen
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

module Plugin.Undo where

import Plugin
import Lambdabot.Parser
import Language.Haskell.Syntax hiding (Module)
import Data.Generics
import qualified Data.Set as Set
import Control.Monad (guard)

$(plugin "Undo")

instance Module UndoModule () where
    moduleCmds   _ = ["undo", "do"]
    moduleHelp _ "undo" = "undo <expr>\nTranslate do notation to Monad operators."
    moduleHelp _ "do" = "do <expr>\nTranslate Monad operators to do notation."
    process_ _ cmd args = ios $ return $ transform f args
     where f = case cmd of
                "undo" -> undo
                "do" -> do'
                _      -> error "unknown command"

findVar :: Data a => a -> String
findVar e = head $ do
                    i <- [0 ..]
                    x <- ['a' .. 'z']
                    let xi = x : replicate i '\''
                    guard $ not $ Set.member xi s
                    return xi
 where s = Set.fromList $ listify (const True :: String -> Bool) e

transform :: (String -> HsExp -> HsExp) -> String -> String
transform f = withParsed $ \e -> everywhere (mkT . f . findVar $ e) e

undo :: String -> HsExp -> HsExp
undo v (HsDo stms) = f stms
 where
    f [HsQualifier e]          = e
    f (HsQualifier e     : xs) = infixed e ">>" $ f xs
    f (HsLetStmt   ds    : xs) = HsLet ds $ f xs
    f (HsGenerator s p e : xs)
        | irrefutable p = infixed e ">>=" $ HsLambda s [p] $ f xs
        | otherwise     = infixed e ">>=" $
                            HsLambda s [pvar v] $
                                HsCase (var v)
                                    [ alt p (f xs)
                                    , alt HsPWildCard $
                                        HsApp
                                            (var "fail")
                                            (HsLit $ HsString "")
                                    ]
        where alt pat x = HsAlt s pat (HsUnGuardedAlt x) []
undo v (HsListComp e stms) = f stms
 where
    f []                       = HsList [e]
    f (HsQualifier g     : xs) = HsIf g (f xs) nil
    f (HsLetStmt   ds    : xs) = HsLet ds $ f xs
    f (HsGenerator s p l : xs)
        | irrefutable p = concatMap' $ HsLambda s [p] $ f xs
        | otherwise     = concatMap' $
                            HsLambda s [pvar v] $
                                HsCase (var v)
                                    [ alt p (f xs)
                                    , alt HsPWildCard nil
                                    ]
        where alt pat x = HsAlt s pat (HsUnGuardedAlt x) []
              concatMap' fun = HsApp (HsApp (var "concatMap") (HsParen fun)) l
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

nil :: HsExp
nil = HsVar list_tycon_name

var :: String -> HsExp
var = HsVar . UnQual . HsIdent

pvar :: String -> HsPat
pvar = HsPVar . HsIdent

do' :: String -> HsExp -> HsExp
do' _ (HsLet ds (HsDo s)) = HsDo (HsLetStmt ds : s)
do' v e@(HsInfixApp l (HsQVarOp (UnQual (HsSymbol op))) r) =
     case op of
         ">>=" ->
             case r of
                 (HsLambda loc [p] (HsDo stms)) -> HsDo (HsGenerator loc p l : stms)
                 (HsLambda loc [HsPVar v1] (HsCase (HsVar (UnQual v2))
                                            [ HsAlt _ p (HsUnGuardedAlt s) []
                                            , HsAlt _ HsPWildCard (HsUnGuardedAlt (HsApp (HsVar (UnQual (HsIdent "fail"))) _)) []
                                            ]))
                           | v1 == v2           -> case s of
                                                       HsDo stms -> HsDo (HsGenerator loc p l : stms)
                                                       _         -> HsDo [HsGenerator loc p l, HsQualifier s]
                 (HsLambda loc [p] s)           -> HsDo [HsGenerator loc p l, HsQualifier s]
                 _ -> HsDo [ HsGenerator undefined (pvar v) l
                           , HsQualifier . app r $ var v]
         ">>" ->
             case r of
                 (HsDo stms) -> HsDo (HsQualifier l : stms)
                 _           -> HsDo [HsQualifier l, HsQualifier r]
         _    -> e
do' _ x = x

-- | 'app' is a smart constructor that inserts parens when the first argument
-- is an infix application.
app :: HsExp -> HsExp -> HsExp
app e@(HsInfixApp {}) f = HsApp (HsParen e) f
app e                 f = HsApp e f
