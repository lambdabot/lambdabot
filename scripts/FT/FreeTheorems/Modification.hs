-- Copyright 2006, Sascha Boehme.




-- | This module defines a function to modify theorems by omitting
--   instantiations in terms.

module FreeTheorems.Modification (
    omitInstantiations,
    omitInstantiationsInUnfoldedRelation
) where



import FreeTheorems.Types



-- | Omits all instantiations in the terms of the given theorem.

omitInstantiations :: Theorem -> Theorem
omitInstantiations theorem =
  case theorem of
    IsElementOf (t1,t2) r   -> IsElementOf (omitIns t1, omitIns t2)
                                           (omitInsRel r)
    ForallPairs p r t       -> ForallPairs p (omitInsRel r)
                                             (omitInstantiations t)
    ForallRelations rv r t  -> ForallRelations rv r (omitInstantiations t)
    ForallFunctions f p r t -> ForallFunctions f p r (omitInstantiations t)
    ForallElements x ty t   -> ForallElements x ty (omitInstantiations t)
    Conjunction t1 t2       -> Conjunction (omitInstantiations t1)
                                           (omitInstantiations t2)
    Implication t1 t2       -> Implication (omitInstantiations t1)
                                           (omitInstantiations t2)



-- | Omits all instantiations in unfolded relations.

omitInstantiationsInUnfoldedRelation :: UnfoldedRelation -> UnfoldedRelation
omitInstantiationsInUnfoldedRelation (UnfoldedRelation rel model set) =
  UnfoldedRelation (omitInsRel rel) model (omitInsSet set)



-- | Omit instantiations in relations.

omitInsRel :: Relation -> Relation
omitInsRel rel =
  case rel of
    RelTerm t types     -> RelTerm (omitIns t) types
    RelVar _            -> rel
    RelLift m c rels    -> RelLift m c (map omitInsRel rels)
    RelLiftList m rel'  -> RelLiftList m (omitInsRel rel')
    RelLiftTuple m rels -> RelLiftTuple m (map omitInsRel rels)
    RelFun m rel1 rel2  -> RelFun m (omitInsRel rel1) (omitInsRel rel2)
    RelForall m rv rel' -> RelForall m rv (omitInsRel rel')



-- | Omits instantiations in unfolded sets.

omitInsSet :: UnfoldedSet -> UnfoldedSet
omitInsSet set =
  case set of
    UnfoldedLift list     -> UnfoldedLift (map omit list)
    UnfoldedLiftList p t  -> UnfoldedLiftList p (omitInstantiations t)
    UnfoldedLiftTuple p t -> UnfoldedLiftTuple p (omitInstantiations t)
    UnfoldedFunction p t  -> UnfoldedFunction p (omitInstantiations t)
    UnfoldedForall p t    -> UnfoldedForall p (omitInstantiations t)
  where
    omit (con, xs, ys, mt) =
      case mt of
        Nothing -> (con, xs, ys, mt)
        Just t  -> (con, xs, ys, Just (omitInstantiations t))



-- | Omits all instantiations in a term.

omitIns :: Term -> Term
omitIns t =
  case t of
    TermVar tv    -> TermVar tv
    TermApp t1 t2 -> TermApp (omitIns t1) (omitIns t2)
    TermIns t _   -> omitIns t
