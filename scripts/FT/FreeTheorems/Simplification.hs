-- Copyright 2006, Sascha Boehme.




-- | This module defines a function to simplify theorems by trivial
--   transformations.

module FreeTheorems.Simplification (
    simplifyTheorem
) where



import FreeTheorems.Types



-- | Simplifies a theorem. It essentially replaces parts like
--
-- >   forall x :: T1.
-- >     forall y :: T2.
-- >       (f x = y)
-- >       ==> theorem
--
--   by
--
-- >   forall x :: T1.
-- >     theorem'
--
--   where @f@ is any function and @theorem'@ is obtained from @theorem@ by
--   replacing each occurrence of @y1@ by @f x1@.

simplifyTheorem :: Theorem -> Theorem
simplifyTheorem theorem =
  case theorem of
    IsElementOf _ _            -> theorem
    ForallPairs p r t          -> ForallPairs p r (simplifyTheorem t)
    ForallRelations rv res t   -> ForallRelations rv res (simplifyTheorem t)
    ForallFunctions f ts res t -> ForallFunctions f ts res (simplifyTheorem t)
    ForallElements x ty t      -> ForallElements x ty (tryToSimplify t)
    Conjunction t1 t2          -> Conjunction (simplifyTheorem t1)
                                              (simplifyTheorem t2)
    Implication t1 t2          -> Implication (simplifyTheorem t1)
                                              (simplifyTheorem t2)



-- | Tries to apply the simplification on the given theorem. This is a helper
--   function for 'simplifyTheorem'.

tryToSimplify :: Theorem -> Theorem
tryToSimplify theorem =
  case theorem of
    ForallElements y _ (Implication (IsElementOf (t1, TermVar tv)
                                                 (RelTerm term _)) t) ->
        if y == tv
          then let term' = case term of
                             TermIns (TermVar (PV "id")) _ -> t1
                             otherwise                     -> TermApp term t1
               in  simplifyTheorem (replaceInTheorem tv term' t)
          else simplifyTheorem theorem
    otherwise -> simplifyTheorem theorem



-- | Replaces every occurrence of a term variable in the given theorem by a
--   term.

replaceInTheorem :: TermVariable -> Term -> Theorem -> Theorem
replaceInTheorem tv term theorem =
  case theorem of
    IsElementOf (t1, t2) rel -> IsElementOf ((replaceInTerm tv term t1)
                                            ,(replaceInTerm tv term t2)) rel
    ForallPairs p r t        -> ForallPairs p r (replaceInTheorem tv term t)
    ForallRelations rv res t -> ForallRelations rv res
                                                (replaceInTheorem tv term t)
    ForallFunctions f ts res t -> ForallFunctions f ts res
                                                (replaceInTheorem tv term t)
    ForallElements x ty t    -> ForallElements x ty (replaceInTheorem tv term t)
    Conjunction t1 t2        -> Conjunction (replaceInTheorem tv term t1)
                                            (replaceInTheorem tv term t2)
    Implication t1 t2        -> Implication (replaceInTheorem tv term t1)
                                            (replaceInTheorem tv term t2)



-- | Replaces every occurrence of a term variable in the given term by a term.

replaceInTerm :: TermVariable -> Term -> Term -> Term
replaceInTerm tv term t =
  case t of
    TermVar tv'   -> if tv' == tv then term else TermVar tv'
    TermApp t1 t2 -> TermApp (replaceInTerm tv term t1)
                             (replaceInTerm tv term t2)
    TermIns t' ty -> TermIns (replaceInTerm tv term t') ty
