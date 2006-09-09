-- Copyright 2006, Sascha Boehme.




-- | This module contains a function to reduce automatically generated
--   relations.

module FreeTheorems.Reduction (
    reduceRelation,

    -- testing interface
    -- testReduction
) where



import FreeTheorems.Types

-- import FreeTheorems.Test.ArbitraryTypes
-- import Test.QuickCheck (quickCheck)



-- | Simplifies relations by replacing lifted relations by identity relations
--   if possible. A lifted relation can be reduced to an identity relation, if
--   all relations it is based on are identity relations. The same is true for
--   functional relations.

reduceRelation :: Relation -> Relation
reduceRelation rel = either id toIdentityRelation $ reduceToId rel



-- | Reduces relations to identity relations. This is a helper function for
--   'reduceRelation'.
--
--   If a lifted relation is only based on identity relations, it can be
--   replaced by a identity relation itself. The same is true for functional
--   relations.
--
--   This functions walks other a given relation and decides on this mechanism
--   if a lifted function should be reduced. It returns @Right t@ if the
--   relation @r@ was reduced to an identity over type @t@. Otherwise,
--   @Left r'@ is returned, where @r'@ is the relation @r@ with possible changes
--   in its subrelations.

reduceToId :: Relation -> Either Relation Type
reduceToId rel =
  case rel of
    RelTerm term typepair   -> processTerm term typepair
    RelVar rv               -> Left rel
    RelLift model con rels  -> either (Left . RelLift model con)
                                      (Right . TypeCon con)
                                      (foldReduceToId rels)

    RelLiftList model rel'  -> either (Left . RelLiftList model)
                                      (Right . TypeList)
                                      (reduceToId rel')

    RelLiftTuple model rels -> either (Left . RelLiftTuple model)
                                      (Right . TypeTuple)
                                      (foldReduceToId rels)

    RelFun model rel1 rel2  -> either (\[r1,r2] -> Left $ RelFun model r1 r2)
                                      (\[t1,t2] -> Right $ TypeFun t1 t2)
                                      (foldReduceToId [rel1, rel2])

    RelForall model rv rel' -> either (Left . RelForall model rv)
                                      (Left . RelForall model rv
                                       . toIdentityRelation)
                                      (reduceToId rel')

  where
    processTerm term typepair =
      case term of
        TermIns (TermVar (PV "id")) ty -> Right ty
        otherwise                      -> Left (RelTerm term typepair)

    foldReduceToId = foldr (\r x -> combine (reduceToId r) x) (Right [])

    combine (Left x) (Left es)   = Left (x : es)
    combine (Left x) (Right es)  = Left (x : (map toIdentityRelation es))
    combine (Right x) (Left es)  = Left ((toIdentityRelation x) : es)
    combine (Right x) (Right es) = Right (x : es)



-- | Returns the identity relation for a given type.

toIdentityRelation :: Type -> Relation
toIdentityRelation t = RelTerm (TermIns (TermVar (PV "id")) t) (t,t)



--------------------------------------------------------------------------------



-- A list of tests for this module.

{-
testReduction = do
  putStr "reduction of relation works ... "
  quickCheck prop_reduceRelation



-- Check that 'simplifyRelationsToId' doesn't change anything when applied
-- twice and that it preserves the structure.

prop_reduceRelation rel =
  (reduceRelation (reduceRelation rel) == reduceRelation rel)
  && compareRelAndRel ((reduceRelation rel), rel)

compareRelAndRel pair =
  case pair of
    (RelTerm t1 _, RelTerm t2 _) -> t1 == t2
    (RelTerm t1 _, _)            -> True

    (RelVar rv1, RelVar rv2) -> rv1 == rv2

    (RelLift m1 c1 rels1, RelLift m2 c2 rels2) ->
        (m1 == m2) && (c1 == c2) && and (map compareRelAndRel $ zip rels1 rels2)

    (RelLiftList m1 rel1, RelLiftList m2 rel2) ->
        (m1 == m2) && compareRelAndRel (rel1, rel2)

    (RelLiftTuple m1 rels1, RelLiftTuple m2 rels2) ->
        (m1 == m2) && and (map compareRelAndRel $ zip rels1 rels2)

    (RelFun m1 rel1 rel1', RelFun m2 rel2 rel2') ->
        (m1 == m2) && compareRelAndRel (rel1, rel2)
                   && compareRelAndRel (rel1', rel2')

    (RelForall m1 rv1 rel1, RelForall m2 rv2 rel2) ->
        (m1 == m2) && (rv1 == rv2) && compareRelAndRel (rel1, rel2)

    otherwise -> False

-}
