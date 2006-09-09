-- Copyright 2006, Sascha Boehme.




-- | This module contains the delta algorithm which transforms a type into a
--   relation.

module FreeTheorems.Delta (
    applyDelta,
    applyDeltaWith,
    Environment,

    -- testing interface
    -- testDelta
) where



import Prelude hiding (lookup)

import FreeTheorems.Preparation
import FreeTheorems.Reduction
import FreeTheorems.TheoremData
import FreeTheorems.Types
import qualified Data.Map as Map (Map, empty, insert, lookup)

-- import FreeTheorems.Test.ArbitraryTypes
import Data.List (nub, delete)
-- import Test.QuickCheck (quickCheck)





-- | Applies the delta algorithm to a type to transform it into a corresponding
--   relation. In the generated relation, subrelations are simplified as much as
--   possible.
--
--   According to the language model used, the correct delta algorithm is
--   chosen. The delta algorithm is essentially a term rewriting, i.e. type
--   terms are transformed in corresponding relations while obeying certain
--   rules of the language subset in use.

applyDelta :: LanguageModel -> Type -> TheoremState Relation
applyDelta model = applyDeltaWith model empty



-- | Applies the delta algorithm to a type to transform it into a corresponding
--   relation. In the generated relation, subrelations are simplified as much as
--   possible.
--
--   According to the language model used, the correct delta algorithm is
--   chosen. The delta algorithm is essentially a term rewriting, i.e. type
--   terms are transformed in corresponding relations while obeying certain
--   rules of the language subset in use.
--
--   This function is a variant of 'applyDelta' where an initial environment can
--   be provided.

applyDeltaWith :: LanguageModel -> Environment -> Type -> TheoremState Relation
applyDeltaWith model env t = do
  rel <- case model of
           BasicModel -> deltaBasic env t
           FixModel   -> deltaFix env t
  return $ reduceRelation rel




-- | Declares the type for environments which are mappings of type variables to
--   relation variables.

type Environment = Map.Map TypeVariable Relation



-- | Creates an empty environment.

empty :: Environment
empty = Map.empty



-- | Updates an environment.

update :: Environment -> TypeVariable -> RelationVariable -> Environment
update env v rv = Map.insert v (RelVar rv) env



-- | Returns the relation variable which is mapped to a type variable.

lookup :: Environment -> TypeVariable -> Relation
lookup env v =
  case Map.lookup v env of
    Just rel -> rel
    -- 'Nothing' can not occur because every types processed in 'delta' is
    -- closed and a 'lookup' may only occur after the corresponding 'update'
    -- (due to the structure of types).





-- | The delta algorithm for the basic model.

deltaBasic :: Environment -> Type -> TheoremState Relation
deltaBasic env t =
  case t of
    TypeBasic b     -> return (RelTerm (TermIns (TermVar (PV "id")) t) (t,t))

    TypeVar v       -> return (lookup env v)

    TypeCon c ts    -> do rels <- mapM (deltaBasic env) ts
                          return (RelLift BasicModel c rels)

    TypeList t1     -> do rel <- deltaBasic env t1
                          return (RelLiftList BasicModel rel)

    TypeUnit        -> return (RelTerm (TermIns (TermVar (PV "id")) t) (t, t))

    TypeTuple ts    -> do rels <- mapM (deltaBasic env) ts
                          return (RelLiftTuple BasicModel rels)

    TypeFun t1 t2   -> do rel1 <- deltaBasic env t1
                          rel2 <- deltaBasic env t2
                          return (RelFun BasicModel rel1 rel2)

    TypeForall v t1 -> do rv  <- newRelationVariable v
                          rel <- deltaBasic (update env v rv) t1
                          return (RelForall BasicModel rv rel)



-- | The delta algorithm for the fix model.

deltaFix :: Environment -> Type -> TheoremState Relation
deltaFix env t =
  case t of
    TypeBasic b     -> return (RelTerm (TermIns (TermVar (PV "id")) t) (t,t))

    TypeVar v       -> return (lookup env v)

    TypeCon c ts    -> do rels <- mapM (deltaFix env) ts
                          return (RelLift FixModel c rels)

    TypeList t1     -> do rel <- deltaFix env t1
                          return (RelLiftList FixModel rel)

    TypeUnit        -> return (RelTerm (TermIns (TermVar (PV "id")) t) (t,t))

    TypeTuple ts    -> do rels <- mapM (deltaFix env) ts
                          return (RelLiftTuple FixModel rels)

    TypeFun t1 t2   -> do rel1 <- deltaFix env t1
                          rel2 <- deltaFix env t2
                          return (RelFun FixModel rel1 rel2)

    TypeForall v t1 -> do rv  <- newRelationVariable v
                          rel <- deltaFix (update env v rv) t1
                          return (RelForall FixModel rv rel)



--------------------------------------------------------------------------------



-- A list of tests for this module.

{-
testDelta = do
  putStr "delta preserves the structure ... "
  quickCheck prop_delta_preserves_structure
  putStr "every relation variable occurs only once ... "
  quickCheck prop_every_rv_only_once
  putStr "every relation variable is bound ... "
  quickCheck prop_every_rv_is_bound
  putStr "relation uses only one model ... "
  quickCheck prop_only_one_model
  putStr "delta reduces the generated relation ... "
  quickCheck prop_delta_reduces_relation
-}



-- Check that delta preserves the structure, i.e. the type is just reflected
-- into a representation by relations.

prop_delta_preserves_structure model t =
   let t' = prepare t
       rel = fst $ execute (PV "t") $ applyDelta model t'
   in  compareTypeAndRelation (rel, t')

compareTypeAndRelation :: (Relation, Type) -> Bool
compareTypeAndRelation pair =
  case pair of
    (RelVar rv, TypeVar v)               -> let R _ v' _ = rv
                                            in  v == v'

    (RelTerm _ _, t')                    -> True

    (RelLift _ c rels, TypeCon c' ts)    -> (c == c')
                                            && and (map compareTR $ zip rels ts)

    (RelLiftList _ rel, TypeList t)      -> compareTR (rel, t)

    (RelLiftTuple _ rels, TypeTuple ts)  -> and (map compareTR $ zip rels ts)

    (RelFun _ rel1 rel2, TypeFun t1 t2)  -> compareTR (rel1, t1) &&
                                            compareTR (rel2, t2)

    (RelForall _ rv rel, TypeForall v t) -> let R _ v' _ = rv
                                            in  v' == v && compareTR (rel, t)

    otherwise      -> False

  where
    compareTR = compareTypeAndRelation



-- Check that every relation variable is bound only once in a quantification,
-- i.e. every relation variable is unique.

prop_every_rv_only_once model t =
  let rel = fst $ execute (PV "t") $ applyDelta model $ prepare t
      rvs = getBoundRelVars rel
  in  rvs == nub rvs

getBoundRelVars rel =
  case rel of
    RelTerm _ _         -> []
    RelVar _            -> []
    RelLift _ _ rels    -> concatMap getBoundRelVars rels
    RelLiftList _ rel'  -> getBoundRelVars rel'
    RelLiftTuple _ rels -> concatMap getBoundRelVars rels
    RelFun _ rel1 rel2  -> getBoundRelVars rel1 ++ getBoundRelVars rel2
    RelForall _ rv rel' -> rv : (getBoundRelVars rel')



-- Check that all relation variables are quantified.

prop_every_rv_is_bound model t =
  let rel = fst $ execute (PV "t") $ applyDelta model $ prepare t
  in  checkRelVars rel == []

checkRelVars rel =
  case rel of
    RelTerm _ _         -> []
    RelVar rv           -> [rv]
    RelLift _ _ rels    -> nub $ concatMap checkRelVars rels
    RelLiftList _ rel'  -> nub $ checkRelVars rel'
    RelLiftTuple _ rels -> nub $ concatMap checkRelVars rels
    RelFun _ rel1 rel2  -> nub $ checkRelVars rel1 ++ checkRelVars rel2
    RelForall _ rv rel' -> delete rv (checkRelVars rel')



-- Check that delta creates relations having only one model.

prop_only_one_model model t =
  let rel = fst $ execute (PV "t") $ applyDelta model $ prepare t
  in  usesModel model rel

usesModel model rel =
  case rel of
    RelTerm _ _         -> True
    RelVar rv           -> True
    RelLift m _ rels    -> m == model && and (map (usesModel model) rels)
    RelLiftList m rel'  -> m == model && usesModel model rel'
    RelLiftTuple m rels -> m == model && and (map (usesModel model) rels)
    RelFun m rel1 rel2  -> m == model && usesModel model rel1
                                      && usesModel model rel2
    RelForall m rv rel' -> m == model && usesModel model rel'



-- Check that delta simplifies its generated relations.

prop_delta_reduces_relation model t =
   let rel = fst $ execute (PV "t") $ applyDelta model $ prepare t
   in  rel == reduceRelation rel



