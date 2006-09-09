-- Copyright 2006, Sascha Boehme.




-- | This module defines a function to extract and unfold lifted relations
--   occurring in theorems.

module FreeTheorems.Lifts (
    extractLiftRelations
) where



import FreeTheorems.Declarations
import FreeTheorems.Delta
import FreeTheorems.Modification
import FreeTheorems.TheoremData
import FreeTheorems.Types
import FreeTheorems.Unfolding
import Control.Monad (liftM, liftM2)
import Control.Monad.State (evalState)
import Data.List (nub)
import qualified Data.Map as Map (fromAscList)
import Data.Maybe (catMaybes)





-- | Creates a list of all lifted relations occurring in the given theorem.
--   Lifted relations are @lift@ relations as well as functional relations and
--   relational abstractions.
--   The lifted relations are returned together with their unfolded description.
--   If the second argument is 'True', all instantiations in the terms of the
--   returned unfolded relations are omitted.

extractLiftRelations :: Theorem -> Bool -> TheoremState [UnfoldedRelation]
extractLiftRelations theorem omitIns = do
  rels <- extractLifts theorem
  lifts <- mapM unfoldLiftRelation $ nub rels
  if omitIns
    then return (map omitInstantiationsInUnfoldedRelation lifts)
    else return lifts



--------------------------------------------------------------------------------



-- | Extracts all lifted relations from a theorem. This is a helper function for
--   'extractLiftRelations'.

extractLifts :: Theorem -> TheoremState [Relation]
extractLifts theorem =
  case theorem of
    IsElementOf _ rel       -> collectLifts rel
    ForallPairs _ rel t     -> liftM2 (++) (collectLifts rel) (extractLifts t)
    ForallRelations _ _ t   -> extractLifts t
    ForallFunctions _ _ _ t -> extractLifts t
    ForallElements _ _ t    -> extractLifts t
    Conjunction t1 t2       -> liftM2 (++) (extractLifts t1) (extractLifts t2)
    Implication t1 t2       -> liftM2 (++) (extractLifts t1) (extractLifts t2)



-- | Returns all lift relations occurring in a relation.

collectLifts :: Relation -> TheoremState [Relation]
collectLifts rel =
  case rel of
    RelTerm _ _         -> return []
    RelVar _            -> return []

    RelLift m con rels  -> do subLifts <- subLifts m con rels
                              lifts <- liftM concat $ mapM collectLifts rels
                              return $ rel : lifts ++ subLifts

    RelLiftList _ rel'  -> do lifts <- collectLifts rel'
                              return $ rel : lifts

    RelLiftTuple _ rels -> do lifts <- liftM concat $ mapM collectLifts rels
                              return $ rel : lifts

    RelFun _ rel1 rel2  -> do lifts1 <- collectLifts rel1
                              lifts2 <- collectLifts rel2
                              return $ rel : lifts1 ++ lifts2

    RelForall _ _ rel'  -> do lifts <- collectLifts rel'
                              return $ rel : lifts



-- | Returns all sublifts occurring in the right-hand side definition of a
--   type constructor. For example, the following datatype has a sublift:
--
-- > data Foo = Bar [Int]
--
--   Here, the function will return the relation for @[Int]@ as the only sublift
--   of @Foo@.
--
--   However, only general type constructors, tuples and lists may occur as
--   sublifts. Functions and type abstractions are forbidden, because their
--   behaviour is not yet defined.

-- The function relies on the fact, that functions and type abstractions may not
-- occur on the right-hand side of data declarations. That way, it can safely
-- use 'delta' (which is also defined for functions and type abstractions) to
-- create relations of types.

subLifts :: LanguageModel
            -> TypeConstructor
            -> [Relation]
            -> TheoremState [Relation]

subLifts model con rels = do
  let Just (DataDecl _ n f) = getDataDecl con
  let vs = map (\i -> "a" ++ show i) [1..n]
  let decls = f (map (\v -> TypeVar v) vs)
  let types = concatMap (\(DataConDecl _ ts) -> ts) decls
  let env = Map.fromAscList $ zip vs rels
  relations <- mapM (applyDeltaWith model env) types
  liftM concat (mapM collectLifts relations)



--------------------------------------------------------------------------------



-- | Unfolds a lift relation. This is a helper function for
--   'extractLiftRelations'.

unfoldLiftRelation :: Relation -> TheoremState UnfoldedRelation
unfoldLiftRelation rel =
  case rel of

-- the following two cases will never occur, see 'collectLifts'
--    RelTerm _ _
--    RelVar _

    RelLift model con rels  -> liftM (pack model) (unfoldLift model con rels)
    RelLiftList model rel'  -> return $ pack model (unfoldLiftList model rel')
    RelLiftTuple model rels -> return $ pack model (unfoldLiftTuple model rels)

    RelFun model rel1 rel2  -> do (f, g) <- newVariablesFor rel
                                  let pair = (TermVar f, TermVar g)
                                  theorem <- unfoldFunction model rel1 rel2 pair
                                  return $ pack model
                                         $ UnfoldedFunction (f,g) theorem

    RelForall model rv rel' -> do (x, y) <- newVariablesFor rel
                                  let pair = (TermVar x, TermVar y)
                                  theorem <- unfoldForall model rv rel' pair
                                  return $ pack model
                                         $ UnfoldedForall (x,y) theorem
  where
    pack = UnfoldedRelation rel



-- | Unfolds a lifted relation of an algebraic datatype. It uses 'delta' on the
--   types occurring on the right-hand side of the according type constructor's
--   definition to generate relations.

unfoldLift :: LanguageModel
              -> TypeConstructor
              -> [Relation]
              -> TheoremState UnfoldedSet

unfoldLift model con rels = do
  let Just (DataDecl _ n f) = getDataDecl con
      vs = map (\i -> "a" ++ show i) [1..n]
      decls = f (map (\v -> TypeVar v) vs)
      env = Map.fromAscList $ zip vs rels

  liftM UnfoldedLift (mapM (unfoldDecl env) decls)

  where
    unfoldDecl env (DataConDecl con types) = do
      let n = length types
          xs = map (TV "x") [1..n]
          ys = map (TV "y") [1..n]
      if n == 0
        then return (con, [], [], Nothing)
        else liftM (\t -> (con, xs, ys, Just t)) (buildTheorem env xs ys types)

    buildTheorem env xs ys types = do
      rels <- mapM (applyDeltaWith model env) types
      let mkTheorem (x,y,r) = IsElementOf (TermVar x, TermVar y) r
          theorems = map mkTheorem (zip3 xs ys rels)
      return $ foldr1 Conjunction theorems



-- | Unfolds a lifted list relation.

unfoldLiftList :: LanguageModel -> Relation -> UnfoldedSet
unfoldLiftList model rel =
  let (x, xs) = (PV "x", PV "xs")
      (y, ys) = (PV "y", PV "ys")
      lift    = RelLiftList model rel
      theorem = Conjunction (IsElementOf (TermVar x, TermVar y) rel)
                            (IsElementOf (TermVar xs, TermVar ys) lift)
  in  UnfoldedLiftList ((x, xs), (y, ys)) theorem



-- | Unfolds a lifted tuple relation.

unfoldLiftTuple :: LanguageModel -> [Relation] -> UnfoldedSet
unfoldLiftTuple model rels =
  let n = length rels
      xs = map (TV "x") [1..n]
      ys = map (TV "y") [1..n]
      mkTheorem (x,y,r) = IsElementOf (TermVar x, TermVar y) r
      theorem = foldr1 Conjunction (map mkTheorem (zip3 xs ys rels))
  in  UnfoldedLiftTuple (xs, ys) theorem

