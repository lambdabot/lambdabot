{-# OPTIONS -w #-}

module Plugin.Free.Expr where

import Plugin.Free.Type
import Plugin.Free.Util

varInExpr :: Var -> Expr -> Bool
varInExpr v (EVar v')
    = v == v'
varInExpr v (EVarOp _ _ v')
    = False
varInExpr v (EApp e1 e2)
    = varInExpr v e1 || varInExpr v e2
varInExpr v (ETyApp e1 t)
    = varInExpr v e1

leftVarOfExpr :: Expr -> Var
leftVarOfExpr (EVar v) = v
leftVarOfExpr (EApp e _) = leftVarOfExpr e
leftVarOfExpr (ETyApp e _) = leftVarOfExpr e

exprSubst :: Var -> Expr -> Expr -> Expr
exprSubst v e e'@(EVar v')
    | v == v'   = e
    | otherwise = e'
exprSubst v e e'@(EVarOp _ _ v')
    = e'
exprSubst v e (EApp e1 e2)
    = EApp (exprSubst v e e1) (exprSubst v e e2)
exprSubst v e (ETyApp e1 t)
    = ETyApp (exprSubst v e e1) t


type Var = String

data Fixity
    = FL | FN | FR
    deriving (Eq, Show)

data Expr
    = EVar Var
    | EVarOp Fixity Int Var
    | EApp Expr Expr
    | ETyApp Expr Type
        deriving (Eq, Show)

precAPP :: Int
precAPP = 10

instance Pretty Expr where
    prettyP _ (EVar v)
        = text v
    prettyP _ (EVarOp _ _ v)
        = lparen <> text v <> rparen
    prettyP p (EApp (EApp (EVarOp fix prec op) e1) e2)
        = prettyParen (p > prec) (
            prettyP pl e1 <+> text op <+> prettyP pr e2
        )
        where
            pl = if fix == FL then prec else prec+1
            pr = if fix == FR then prec else prec+1
    prettyP p (EApp e1 e2)
        = prettyParen (p > precAPP) (
            prettyP precAPP e1 <+> prettyP (precAPP+1) e2
        )
    prettyP p (ETyApp e t)
        = prettyP precAPP e

-- vim: ts=4:sts=4:expandtab:ai
