-- Copyright 2006, Sascha Boehme.




-- | This module defines a function to unfold relations generated from types to
--   theorems.

module FreeTheorems.Unfolding (
    unfold,
    unfoldFunction,
    unfoldForall,
    getTypesOf,

    -- testing interface
    -- testUnfolding
) where



import FreeTheorems.Simplification
import FreeTheorems.TheoremData
import FreeTheorems.Types
import Control.Monad (liftM)

import FreeTheorems.Delta
import FreeTheorems.Preparation
-- import FreeTheorems.Test.ArbitraryTypes
-- import Test.QuickCheck (quickCheck)





-- | Unfolds a relation to a theorem.
--   It replaces all functional relations and relation abstractions by logical
--   statements and conditions while the other relations are transformed in
--   is-element-of predicates using relations on their right-hand side.
--
--   The returned theorem is simplified using 'simplifyTheorem'.

unfold :: TermVariable
                -- ^ The name of the type from which the theorem is to be
                --   generated.
          -> Relation
                -- ^ Relation which was generated from the type.
          -> TheoremState Theorem
                -- ^ Returns the generated theorem in the corresponding theorem
                --   state.

unfold tv rel =
  liftM simplifyTheorem (unfoldRelation rel (TermVar tv, TermVar tv))





-- | Unfolds the given relation to a theorem. This is a helper function for
--   'unfold'.
--
--   Basic relations are transformed into is-element-of predicates while
--   functional relations and relation abstractions are replaced by logical
--   expressions. See also 'unfoldFunction' and 'unfoldForall'.

unfoldRelation :: Relation -> (Term, Term) -> TheoremState Theorem
unfoldRelation rel (t1, t2) =
  case rel of
    RelTerm _ _             -> return $ IsElementOf (t1,t2) rel
    RelVar _                -> return $ IsElementOf (t1,t2) rel
    RelLift _ _ _           -> return $ IsElementOf (t1,t2) rel
    RelLiftList _ _         -> return $ IsElementOf (t1,t2) rel
    RelLiftTuple _ _        -> return $ IsElementOf (t1,t2) rel
    RelFun model rel1 rel2  -> unfoldFunction model rel1 rel2 (t1,t2)
    RelForall model rv rel1 -> unfoldForall model rv rel1 (t1,t2)



-- | Unfolds a functional relation to a logical expression.

unfoldFunction :: LanguageModel
                  -> Relation
                  -> Relation
                  -> (Term, Term)
                  -> TheoremState Theorem

unfoldFunction model rel1 rel2 (t1,t2) = do
  (x,y) <- newVariablesFor rel1
  let (tx,ty) = (TermVar x, TermVar y)
  theorem <- unfoldRelation rel2 (TermApp t1 tx, TermApp t2 ty)
  case rel1 of
    RelTerm t (t1,t2) -> return $ ForallElements x t1
                                $ ForallElements y t2
                                $ Implication (IsElementOf (tx,ty) rel1) theorem

    RelVar _          -> return $ ForallPairs (x,y) rel1 theorem
    RelLift _ _ _     -> return $ ForallPairs (x,y) rel1 theorem
    RelLiftList _ _   -> return $ ForallPairs (x,y) rel1 theorem
    RelLiftTuple _ _  -> return $ ForallPairs (x,y) rel1 theorem

    RelFun _ rel3 rel4  -> do let (t1, t2) = getTypesOf rel3
                              let (t3, t4) = getTypesOf rel4
                              theorem1 <- unfoldFunction model rel3 rel4 (tx,ty)
                              return $ ForallElements x (TypeFun t1 t3)
                                     $ ForallElements y (TypeFun t2 t4)
                                     $ Implication theorem1 theorem

    RelForall _ rv rel3 -> do let (t1, t2) = getTypesOf rel1
                              theorem1 <- unfoldForall model rv rel3 (tx,ty)
                              return $ ForallElements x t1
                                     $ ForallElements y t2
                                     $ Implication theorem1 theorem



-- | Unfolds a relation abstraction to a logical expression.
--   Depending on the language model used, the expression may contain certain
--   restriction annotations.

unfoldForall :: LanguageModel
                -> RelationVariable
                -> Relation
                -> (Term, Term)
                -> TheoremState Theorem

unfoldForall model rv rel (t1,t2) = do
  let R _ _ (ttv1, ttv2) = rv
  let pair = (TermIns t1 (TypeTermVar ttv1), TermIns t2 (TypeTermVar ttv2))

  let res = case model of
              BasicModel -> []
              FixModel   -> [IsStrictAndContinuous rv]

  liftM (ForallRelations rv res) (unfoldRelation rel pair)



--------------------------------------------------------------------------------



-- | Every relation is defined over a pair of closed types. This function
--   returns exactly that pair of types for an arbitrary relation. Thus, it acts
--   like an inverse delta function, although this is not fully true.

getTypesOf :: Relation -> (Type, Type)
getTypesOf rel = getTypes [] rel



-- | Computes the pair of types for a relation. This function is a helper
--   function for 'getTypesOf'.
--
--   The second argument contains a list of relation variables which should
--   be replaced by corresponding type variables (used to process abstractions).
--   Otherwise, relation variables are replaced by the according type term
--   variables.

getTypes :: [RelationVariable] -> Relation -> (Type, Type)
getTypes rvs rel =
  case rel of
    RelTerm _ typepair      -> typepair

    RelVar rv               -> let R _ v (ttv1, ttv2) = rv
                               in  if rv `elem` rvs
                                     then (TypeVar v, TypeVar v)
                                     else (TypeTermVar ttv1, TypeTermVar ttv2)

    RelLift model c rels    -> let (ts1, ts2) = unzip $ map (getTypes rvs) rels
                               in  (TypeCon c ts1, TypeCon c ts2)

    RelLiftList model rel'  -> let (t1, t2) = getTypes rvs rel'
                               in  (TypeList t1, TypeList t2)

    RelLiftTuple model rels -> let (ts1, ts2) = unzip $ map (getTypes rvs) rels
                               in  (TypeTuple ts1, TypeTuple ts2)

    RelFun model rel1 rel2  -> let (t1, t2) = getTypes rvs rel1
                                   (t3, t4) = getTypes rvs rel2
                               in  (TypeFun t1 t3, TypeFun t2 t4)

    RelForall model rv rel' -> let (t1, t2) = getTypes (rv:rvs) rel'
                                   R _ v _ = rv
                               in  (TypeForall v t1, TypeForall v t2)



--------------------------------------------------------------------------------



-- A list of tests for this module.

{-
testUnfolding = do
  putStr "unfold replaces functions and abstractions ... "
  quickCheck prop_unfold
  putStr "every variable is quantified ... "
  quickCheck prop_variables_are_quantified
-}



-- Check that after expansion, there are no function type relations and
-- type abstraction relations anymore.

prop_unfold model t =
  let tv = PV "t"
      theorem = fst $ execute tv (applyDelta model (prepare t) >>= unfold tv)
  in  hasNoFunAndForall theorem


hasNoFunAndForall theorem =
  case theorem of
    IsElementOf _ rel     -> isNotFunOrForall rel
    ForallPairs _ rel t   -> isNotFunOrForall rel && hasNoFunAndForall t
    ForallRelations _ _ t -> hasNoFunAndForall t
    ForallElements _ _ t  -> hasNoFunAndForall t
    Conjunction t1 t2     -> hasNoFunAndForall t1 && hasNoFunAndForall t2
    Implication t1 t2     -> hasNoFunAndForall t1 && hasNoFunAndForall t2

isNotFunOrForall rel =
  case rel of
    RelTerm _ _      -> True
    RelVar _         -> True
    RelLift _ _ _    -> True
    RelLiftList _ _  -> True
    RelLiftTuple _ _ -> True
    RelFun _ _ _     -> False
    RelForall _ _ _  -> False



-- Check that every variable is quantified after expansion.

prop_variables_are_quantified model t =
  let tv = PV "t"
      theorem = fst $ execute tv (applyDelta model (prepare t) >>= unfold tv)
  in  cqTheorem [PV "t", PV "id"] [] [] theorem


cqTheorem tvs ttvs rvs theorem =
  case theorem of
    IsElementOf (t1,t2) rel     -> cqTerm tvs ttvs t1 && cqTerm tvs ttvs t2
                                   && cqRelation tvs ttvs rvs rel

    ForallPairs (tv1,tv2) rel t -> cqRelation tvs ttvs rvs rel
                                   && cqTheorem (tv1:tv2:tvs) ttvs rvs t

    ForallRelations rv res t    -> let R _ _ (ttv1,ttv2) = rv
                                       ttvs' = ttv1:ttv2:ttvs
                                   in cqRestriction tvs (rv:rvs) res
                                      && cqTheorem tvs ttvs' (rv:rvs) t

    ForallElements tv ty t      -> cqType [] ttvs ty
                                   && cqTheorem (tv:tvs) ttvs rvs t

    Conjunction t1 t2           -> cqTheorem tvs ttvs rvs t1
                                   && cqTheorem tvs ttvs rvs t2

    Implication t1 t2           -> cqTheorem tvs ttvs rvs t1
                                   && cqTheorem tvs ttvs rvs t2


cqRestriction tvs rvs [] = True
cqRestriction tvs rvs (res:ress) = cqRestriction tvs rvs ress &&
  case res of
    IsStrict tv              -> tv `elem` tvs
    IsStrictAndContinuous rv -> rv `elem` rvs


cqTerm tvs ttvs t =
  case t of
    TermVar tv    -> tv `elem` tvs
    TermApp t1 t2 -> cqTerm tvs ttvs t1 && cqTerm tvs ttvs t2
    TermIns t' ty -> cqTerm tvs ttvs t' && cqType [] ttvs ty

cqRelation tvs ttvs rvs rel =
  case rel of
    RelTerm t (ty1, ty2) -> cqTerm tvs ttvs t
                            && cqType [] ttvs ty1 && cqType [] ttvs ty2
    RelVar rv            -> rv `elem` rvs
    RelLift _ _ rels     -> and $ map (cqRelation tvs ttvs rvs) rels
    RelLiftList _ rel'   -> cqRelation tvs ttvs rvs rel'
    RelLiftTuple _ rels  -> and $ map (cqRelation tvs ttvs rvs) rels
    RelFun _ rel1 rel2   -> cqRelation tvs ttvs rvs rel1
                            && cqRelation tvs ttvs rvs rel2
    RelForall _ rv rel'  -> cqRelation tvs ttvs (rv:rvs) rel'

cqType vs ttvs t =
  case t of
    TypeBasic _     -> True
    TypeVar v       -> v `elem` vs
    TypeTermVar ttv -> ttv `elem` ttvs
    TypeCon _ ts    -> and $ map (cqType vs ttvs) ts
    TypeList t'     -> cqType vs ttvs t'
    TypeUnit        -> True
    TypeTuple ts    -> and $ map (cqType vs ttvs) ts
    TypeFun t1 t2   -> cqType vs ttvs t1 && cqType vs ttvs t2
    TypeForall v t' -> cqType (v:vs) ttvs t'





zz = TypeForall "a" (TypeFun (TypeBasic Char) (TypeTuple [TypeBasic Float,TypeVar "b"
  ,TypeBasic Char,TypeCon "String" [],TypeCon "String" [],TypeVar "b",TypeBasic
  Double,TypeVar "c",TypeVar "b",TypeCon "String" [],TypeBasic Int]))

zzz = fst $ execute (PV "t") (applyDelta BasicModel (prepare zz) >>= unfold (PV "t"))
