{-# OPTIONS -w #-}

module Plugin.Free.Theorem where

import Plugin.Free.Type
import Plugin.Free.Expr
import Plugin.Free.Util

data Theorem
    = ThForall Var Type Theorem
    | ThImplies Theorem Theorem
    | ThEqual Expr Expr
    | ThAnd Theorem Theorem
        deriving (Eq,Show)

precIMPLIES, precAND :: Int
precIMPLIES = 5
precAND = 3

instance Pretty Theorem where
    prettyP p t = prettyTheorem p False t


prettyTheorem :: Int -> Bool -> Theorem -> Doc
prettyTheorem p fa th@(ThForall v t p1)
    | fa        = prettyForall p [v] p1
    | otherwise = prettyP p p1
prettyTheorem p fa (ThImplies p1 p2)
    = prettyParenIndent (p > precIMPLIES) (
        prettyTheorem (precIMPLIES+1) True p1
        $$ nest (-1) (text "=>")
        $$ prettyTheorem precIMPLIES fa p2
    )
prettyTheorem _ _ (ThEqual e1 e2)
    = prettyP 0 e1 <+> text "=" <+> prettyP 0 e2
prettyTheorem p fa (ThAnd e1 e2)
    = prettyParenIndent (p > precAND) (
        prettyTheorem (precAND+1) fa e1 $$ text "&&"
        $$ prettyTheorem precAND fa e2
    )

prettyForall :: Int -> [Var] -> Theorem -> Doc
prettyForall p vs (ThForall v t p1)
    = prettyForall p (v:vs) p1
prettyForall p vs th
    = parens (
        text "forall" <+> hsep [ text v | v <- reverse vs ] <> text "."
        <+> prettyTheorem 0 True th
    )

varInTheorem :: Var -> Theorem -> Bool
varInTheorem v (ThForall v' t p)
    = v /= v' && varInTheorem v p
varInTheorem v (ThImplies p1 p2)
    = varInTheorem v p1 || varInTheorem v p2
varInTheorem v (ThEqual e1 e2)
    = varInExpr v e1 || varInExpr v e2
varInTheorem v (ThAnd e1 e2)
    = varInTheorem v e1 || varInTheorem v e2

applySimplifierTheorem :: (Theorem -> Theorem) -> (Theorem -> Theorem)
applySimplifierTheorem s (ThForall v t p)
    = ThForall v t (s p)
applySimplifierTheorem s (ThImplies p1 p2)
    = ThImplies (s p1) (s p2)
applySimplifierTheorem s p@(ThEqual _ _)
    = p
applySimplifierTheorem s p@(ThAnd p1 p2)
    = ThAnd (s p1) (s p2)

peepholeSimplifyTheorem :: Theorem -> Theorem
peepholeSimplifyTheorem
    = peepholeSimplifyTheorem' . applySimplifierTheorem peepholeSimplifyTheorem

peepholeSimplifyTheorem' :: Theorem -> Theorem
peepholeSimplifyTheorem' (ThForall v t p)
    = case varInTheorem v p of
            True  -> ThForall v t p
            False -> p
peepholeSimplifyTheorem' p@(ThAnd e1 e2)
    = foldr1 ThAnd (flattenAnd e1 . flattenAnd e2 $ [])
    where
        flattenAnd (ThAnd e1 e2) = flattenAnd e1 . flattenAnd e2
        flattenAnd e = (e:)
peepholeSimplifyTheorem' p
    = p

peepholeSimplifyExpr :: Expr -> Expr
peepholeSimplifyExpr
    = peepholeSimplifyExpr' . applySimplifierExpr peepholeSimplifyExpr

peepholeSimplifyExpr' :: Expr -> Expr
peepholeSimplifyExpr' (EApp (EBuiltin BId) e2)
    = e2
peepholeSimplifyExpr' (EApp (EBuiltin (BMap _)) (EBuiltin BId))
    = EBuiltin BId
peepholeSimplifyExpr' e
    = e

foldEquality :: Theorem -> Theorem
foldEquality p@(ThForall _ _ _)
    = case foldEquality' p [] of
        Just p' -> p'
        Nothing -> applySimplifierTheorem foldEquality p
    where
        foldEquality' (ThForall v t p) vts
            = foldEquality' p ((v,t):vts)
        foldEquality' (ThImplies (ThEqual (EVar v) e2) p) vts
            | v `elem` map fst vts
                = foldEquality'' vts (theoremSubst v e2 p)
        foldEquality' (ThImplies (ThEqual e1 (EVar v)) p) vts
            | v `elem` map fst vts
                = foldEquality'' vts (theoremSubst v e1 p)
        foldEquality' _ vts
            = Nothing

        foldEquality'' [] e
            = Just e
        foldEquality'' ((v,t):vts) e
            = foldEquality'' vts (ThForall v t e)

foldEquality p
    = applySimplifierTheorem foldEquality p

tryCurrying :: Theorem -> Theorem
tryCurrying p@(ThForall _ _ _)
    = case tryCurrying' p [] of
        Just p' -> p'
        Nothing -> applySimplifierTheorem tryCurrying p
    where
        tryCurrying' (ThForall v t p) vts
            = tryCurrying' p ((v,t):vts)
        tryCurrying' (ThEqual e1 e2) vts
            = case (traverseRight ECDot e1, traverseRight ECDot e2) of
                ((ctx1, EVar v1), (ctx2, EVar v2))
                    | v1 == v2 && v1 `elem` map fst vts
                        && not (varInCtx v1 ctx1) && not (varInCtx v2 ctx2)
                        -> tryCurrying'' vts (ThEqual (untraverse ctx1)
                                                      (untraverse ctx2))
                _       -> Nothing
        tryCurrying' _ _
            = Nothing

        traverseRight ctx (EApp e1 e2)
            = traverseRight (ECAppR e1 ctx) e2
        traverseRight ctx e
            = (ctx, e)

        untraverse ECDot = EBuiltin BId
        untraverse (ECAppR e1 ECDot)
            = e1
        untraverse (ECAppR e1 ctx)
            = EApp (EApp (EVarOp FR 9 ".") (untraverse ctx)) e1
        tryCurrying'' [] e
            = Just e
        tryCurrying'' ((v,t):vts) e
            = tryCurrying'' vts (ThForall v t e)

tryCurrying p
    = applySimplifierTheorem tryCurrying p

theoremSimplify :: Theorem -> Theorem
theoremSimplify
    = iterateUntilFixpoint
        (foldEquality
        . iterateUntilFixpoint peephole
        . tryCurrying
        . iterateUntilFixpoint peephole
        )
    where
        iterateUntilFixpoint s t
            = findFixpoint (iterate s t)

        peephole t = findFixpoint (iterate peepholeSimplifyTheorem t)

        findFixpoint (x1:xs@(x2:_))
            | x1 == x2  = x2
            | otherwise = findFixpoint xs

theoremSubst :: Var -> Expr -> Theorem -> Theorem
theoremSubst v e (ThForall f t p)
    = ThForall f t (theoremSubst v e p)
theoremSubst v e (ThImplies p1 p2)
    = ThImplies (theoremSubst v e p1) (theoremSubst v e p2)
theoremSubst v e (ThEqual e1 e2)
    = ThEqual (exprSubst v e e1) (exprSubst v e e2)
theoremSubst v e (ThAnd p1 p2)
    = ThAnd (theoremSubst v e p1) (theoremSubst v e p2)

-- vim: ts=4:sts=4:expandtab:ai
