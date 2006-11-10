{-# OPTIONS -w #-}

module Plugin.Free.Expr where

import Plugin.Free.Type
import Plugin.Free.Util

varInExpr :: Var -> Expr -> Bool
varInExpr v (EBuiltin _)
    = False
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
exprSubst v e e'@(EBuiltin _)
    = e'
exprSubst v e e'@(EVar v')
    | v == v'   = e
    | otherwise = e'
exprSubst v e e'@(EVarOp _ _ v')
    | v == v'   = e
    | otherwise = e'
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
    | EBuiltin Builtin
    | EVarOp Fixity Int Var
    | EApp Expr Expr
    | ETyApp Expr Type
        deriving (Eq, Show)

data Builtin
    = BMap TyName
    | BId
    | BProj Int Int
    | BMapTuple Int
    | BArr
        deriving (Eq, Show)

data ExprCtx
    = ECDot
    | ECAppL ExprCtx Expr
    | ECAppR Expr ExprCtx
    | ECTyApp ExprCtx Type
        deriving (Eq, Show)

applySimplifierExpr :: (Expr -> Expr) -> (Expr -> Expr)
applySimplifierExpr s (EApp e1 e2)
    = EApp (s e1) (s e2)
applySimplifierExpr s (ETyApp e t)
    = ETyApp (s e) t
applySimplifierExpr s e
    = e

unzipExpr :: Expr -> ExprCtx -> Expr
unzipExpr e ECDot = e
unzipExpr e (ECAppL c e2) = unzipExpr (EApp e e2) c
unzipExpr e (ECAppR e1 c) = unzipExpr (EApp e1 e) c
unzipExpr e (ECTyApp c t) = unzipExpr (ETyApp e t) c

varInCtx :: Var -> ExprCtx -> Bool
varInCtx v ECDot
    = False
varInCtx v (ECAppL c e2)
    = varInCtx v c || varInExpr v e2
varInCtx v (ECAppR e1 c)
    = varInCtx v c || varInExpr v e1
varInCtx v (ECTyApp c _)
    = varInCtx v c

precAPP :: Int
precAPP = 10

instance Pretty Expr where
    prettyP p (EBuiltin b)
        = prettyP p b
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

instance Pretty Builtin where
    prettyP p (BMap "[]")   = text "$map"
    prettyP p (BMap c)      = text ("$map_" ++ c)
    prettyP p BId           = text "$id"
    prettyP p (BProj 2 1)   = text "$fst"
    prettyP p (BProj 2 2)   = text "$snd"
    prettyP p (BProj 3 1)   = text "$fst3"
    prettyP p (BProj 3 2)   = text "$snd3"
    prettyP p (BProj 3 3)   = text "$thd3"
    prettyP p (BProj l i)   = text ("$proj_" ++ show l ++ "_" ++ show i)
    prettyP p (BMapTuple 2) = text "$map_Pair"
    prettyP p (BMapTuple 3) = text "$map_Triple"
    prettyP p (BMapTuple n) = text $ "$map_Tuple" ++ show n
    prettyP p BArr          = text "$arr"

-- vim: ts=4:sts=4:expandtab:ai
