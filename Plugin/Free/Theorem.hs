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
    prettyP p (ThForall f t p1)
        = prettyP p p1
    prettyP p (ThImplies p1 p2)
        = prettyParenIndent (p > precIMPLIES) (
            prettyP (precIMPLIES+1) p1
            $$ nest (-1) (text "=>")
            $$ prettyP precIMPLIES p2
        )
    prettyP _ (ThEqual e1 e2)
        = prettyP 0 e1 <+> text "=" <+> prettyP 0 e2
    prettyP p (ThAnd e1 e2)
        = prettyParenIndent (p > precAND) (
            prettyP (precAND+1) e1 $$ text "&&" $$ prettyP precAND e2
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
peepholeSimplifyTheorem (ThForall v t p)
    = let p' = peepholeSimplifyTheorem p
      in case varInTheorem v p' of
            True  -> ThForall v t p'
            False -> p'
peepholeSimplifyTheorem (ThImplies p1 p2)
    = case (peepholeSimplifyTheorem p1,peepholeSimplifyTheorem p2) of
--        (ThAnd p1' p2',p3')
--            -> ThImplies p1' (ThImplies p2' p3')
        (p1',p2')
            -> ThImplies p1' p2'
peepholeSimplifyTheorem p@(ThEqual _ _)
    = p
peepholeSimplifyTheorem p@(ThAnd e1 e2)
    = let e1' = peepholeSimplifyTheorem e1
          e2' = peepholeSimplifyTheorem e2
      in foldr1 ThAnd (flattenAnd e1' . flattenAnd e2' $ [])
    where
        flattenAnd (ThAnd e1 e2) = flattenAnd e1 . flattenAnd e2
        flattenAnd e = (e:)

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
        tryCurrying' (ThEqual (EApp f1 (EVar v1))
                              (EApp f2 (EVar v2))) vts
            | v1 == v2 && v1 `elem` map fst vts
                && not (varInExpr v1 f1) && not (varInExpr v2 f2)
                = tryCurrying'' vts (ThEqual f1 f2)
        tryCurrying' (ThEqual (EApp f1 (EApp g1 (EVar v1)))
                              (EApp f2 (EApp g2 (EVar v2)))) vts
            | v1 == v2 && v1 `elem` map fst vts
                && not (varInExpr v1 f1) && not (varInExpr v1 g1)
                && not (varInExpr v2 f2) && not (varInExpr v2 g2)
                = tryCurrying'' vts
                    (ThEqual (EApp (EApp (EVarOp FR 9 ".") f1) g1)
                             (EApp (EApp (EVarOp FR 9 ".") f2) g2))
        tryCurrying' _ _ = Nothing

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
