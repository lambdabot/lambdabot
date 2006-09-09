-- Copyright 2006, Sascha Boehme.




-- | This module defines function which are used in specializing relations to
--   functions.

module FreeTheorems.Specialization (
    extractRelationVariables,
    replaceRelationVariable
) where



import FreeTheorems.Simplification
import FreeTheorems.TheoremData
import FreeTheorems.Types
import FreeTheorems.Unfolding
import Control.Monad (liftM)



-- | Returns all relation variables of a theorem which can be specialized to a
--   function.
--   Relation variables being quantified on the left-hand side of an implication
--   can not be specialized. However, relation variables being quantified like
--
-- >   ((forall R. theorem) ==> theorem1) ==> theorem2
--
--   can be specialized. Thus, the position relative to implications decides
--   whether a relation variable is returned by this function.

extractRelationVariables :: Theorem -> [RelationVariable]
extractRelationVariables theorem = extractRelVars True theorem



-- | Returns all relation variables of a theorem which can be specialized to a
--   function. This is a helper function for 'extractRelationVariables' and uses
--   its first argument to decide if a relation variable can be specialized to a
--   function.

extractRelVars :: Bool -> Theorem -> [RelationVariable]
extractRelVars takeRVs theorem =
  case theorem of
    IsElementOf _ _         -> []
    ForallPairs _ _ t       -> extractRelVars takeRVs t
    ForallRelations rv _ t  -> if takeRVs
                                 then rv : (extractRelVars takeRVs t)
                                 else extractRelVars takeRVs t
    ForallFunctions _ _ _ t -> extractRelVars takeRVs t
    ForallElements _ _ t    -> extractRelVars takeRVs t
    Conjunction t1 t2       -> extractRelVars takeRVs t1
                               ++ extractRelVars takeRVs t2
    Implication t1 t2       -> extractRelVars (not takeRVs) t1
                               ++ extractRelVars takeRVs t2



--------------------------------------------------------------------------------



-- | Replaces a relation variable found by 'extractRelationVariables' in the
--   given theorem. A new function symbol is created to replace the relation
--   variable. The returned theorem is simplified by 'simplifyTheorem'.

replaceRelationVariable :: Theorem -> RelationVariable -> TheoremState Theorem
replaceRelationVariable theorem rv = do
  f <- newRelationAsFunctionVariable
  return $ simplifyTheorem (replaceRelVar rv f theorem)



-- | Replaces every occurrence of a relation variable by the given term
--   variable. This is a helper function for 'replaceRelationVariable'.

replaceRelVar :: RelationVariable -> TermVariable -> Theorem -> Theorem
replaceRelVar rv f theorem =
  case theorem of
    IsElementOf p r       -> IsElementOf p (replaceInRel rv f [] r)

    ForallPairs p r t     -> let r' = replaceInRel rv f [] r
                                 t' = replaceRelVar rv f t
                             in  adjustForallPairs p r' t'

    ForallRelations rv' res t -> let t'   = replaceRelVar rv f t
                                     res' = map (updateRestriction f) res
                                     R _ _ types = rv
                                 in  if rv == rv'
                                       then ForallFunctions f types res' t'
                                       else ForallRelations rv'     res  t'

    ForallFunctions f' types res t -> ForallFunctions f' types res
                                                      (replaceRelVar rv f t)

    ForallElements x ty t -> ForallElements x ty (replaceRelVar rv f t)

    Conjunction t1 t2     -> Conjunction (replaceRelVar rv f t1)
                                         (replaceRelVar rv f t2)

    Implication t1 t2     -> Implication (replaceRelVar rv f t1)
                                         (replaceRelVar rv f t2)



-- | Takes a restriction for a relation variable and transforms it into a
--   restriction for the given term variable.

updateRestriction :: TermVariable -> Restriction -> Restriction
updateRestriction f res =
  case res of
    IsStrictAndContinuous _ -> IsStrict f
    otherwise               -> res



-- | Replaces every occurrence of the given relation variable by the given term
--   variable in a relation.
--   The third argument gives a list of relation variables bound in the current
--   environment.

replaceInRel :: RelationVariable
                -> TermVariable
                -> [RelationVariable]
                -> Relation
                -> Relation

replaceInRel rv f rvs rel =
  case rel of
    RelTerm _ _          -> rel
    RelVar rv'           -> if (rv' `elem` rvs) || (rv' /= rv)
                              then RelVar rv'
                              else let R _ _ (ttv1, ttv2) = rv
                                       types = ( TypeTermVar ttv1
                                               , TypeTermVar ttv2)
                                   in  RelTerm (TermVar f) types
    RelLift m c rels     -> RelLift m c (map (replaceInRel rv f rvs) rels)
    RelLiftList m rel'   -> adjustLiftList m (replaceInRel rv f rvs rel')
    RelLiftTuple m rels  -> RelLiftTuple m (map (replaceInRel rv f rvs) rels)
    RelFun m rel1 rel2   -> RelFun m (replaceInRel rv f rvs rel1)
                                     (replaceInRel rv f rvs rel2)
    RelForall m rv' rel' -> RelForall m rv' (replaceInRel rv f (rv':rvs) rel')



-- | Creates a proper notation of a lifted list relation. If the argument
--   relation is a function, the list relation is replaced by @map@.

adjustLiftList :: LanguageModel -> Relation -> Relation
adjustLiftList model rel =
  case rel of
    RelTerm t (ty1, ty2) -> let tmap  = TermVar (PV "map")
                                tmap' = (TermIns (TermIns tmap ty1) ty2)
                            in  RelTerm (TermApp tmap' t)
                                        (TypeList ty1, TypeList ty2)
    otherwise            -> RelLiftList model rel



-- | Creates a proper notation for abstractions over term variables. If the
--   corresponding relation is a function, then the term variables abstraction
--   uses a different notation.

adjustForallPairs :: (TermVariable, TermVariable)
                     -> Relation
                     -> Theorem
                     -> Theorem

adjustForallPairs (tv1, tv2) rel theorem =
  case rel of
    RelTerm _ _ -> let (ty1, ty2)  = getTypesOf rel
                   in  ForallElements tv1 ty1
                       $ ForallElements tv2 ty2
                       $ Implication
                           (IsElementOf (TermVar tv1, TermVar tv2) rel)
                           theorem
    otherwise   -> ForallPairs (tv1, tv2) rel theorem

