-- Copyright 2006, Sascha Boehme.




-- | This module contains functions to process types before a theorem can be
--   generated out of them.

module FreeTheorems.Preparation (
    prepare,

    -- testing interface
    -- testPreparation
) where



import FreeTheorems.Declarations (TypeDeclaration(..), getTypeDecl)
import FreeTheorems.Types
import Data.List (nub, delete)

-- import FreeTheorems.Test.ArbitraryTypes
-- import Test.QuickCheck (quickCheck)





-- | Prepares a type before a theorem can be generated out of it.

prepare :: Type -> Type
prepare = replaceTypeSynonyms . closure





-- | Computes the closure for a named type term. The returned type term contains
--   no free type variables anymore.

closure :: Type -> Type
closure t = foldr (\v t -> TypeForall v t) t (freeVariables t)



-- | Computes the free type variables for a type term.
--   This is a helper function for 'closure'.

freeVariables :: Type -> [TypeVariable]
freeVariables t =
  case t of
    TypeBasic _     -> []
    TypeVar v       -> [v]
    TypeTermVar _   -> []
    TypeCon _ ts    -> nub $ concat $ map freeVariables ts
    TypeList t1     -> freeVariables t1
    TypeUnit        -> []
    TypeTuple ts    -> nub $ concat $ map freeVariables ts
    TypeFun t1 t2   -> nub $ concat $ map freeVariables [t1, t2]
    TypeForall v t1 -> delete v (freeVariables t1)





-- | Replaces all type synonyms by its declaration's right-hand side.
--   Only type synonyms occurring in the set of user-defined types are replaced.
--   However, there may be no other type synonyms in the type term, anyway.

replaceTypeSynonyms :: Type -> Type
replaceTypeSynonyms t =
  let replaceTS = replaceTypeSynonyms
  in  case t of
        TypeBasic _     -> t
        TypeVar _       -> t
        TypeTermVar _   -> t
        TypeCon c ts    -> replaceByDecl c ts
        TypeList t1     -> TypeList (replaceTS t1)
        TypeUnit        -> t
        TypeTuple ts    -> TypeTuple (map replaceTS ts)
        TypeFun t1 t2   -> TypeFun (replaceTS t1) (replaceTS t2)
        TypeForall v t1 -> TypeForall v (replaceTS t1)




-- | Replaces a type constructor by its declaration's right-hand side, if the
--   type constructor belongs to a type synonym. Otherwise calls
--   'replaceTypeSynonyms' on the subtypes.

replaceByDecl :: TypeConstructor -> [Type] -> Type
replaceByDecl con ts =
  case getTypeDecl con of
    Nothing   -> TypeCon con (map replaceTypeSynonyms ts)
    Just decl -> let TypeDecl _ _ rhs = decl
                 in  replaceTypeSynonyms (rhs ts)



--------------------------------------------------------------------------------



-- A list of tests for this module.

{-
testPreparation = do
  putStr "closure of types works ... "
  quickCheck prop_closure
  putStr "type synonyms are replaced correctly ... "
  quickCheck prop_replaceTypeSynonyms
  putStr "preparation of types works ... "
  quickCheck prop_prepare

-}


-- A closed type term does not contain any free variable.

prop_closure t =
     (freeVariables (closure t) == [])
  && (closure t == closure (closure t))



-- Check that there are no type synonyms anymore and that further applications
-- of 'replaceTypeSynonyms' don't change the type anymore.

prop_replaceTypeSynonyms t =
     (hasNoTypeSynonyms $ replaceTypeSynonyms t)
  && (replaceTypeSynonyms t == replaceTypeSynonyms (replaceTypeSynonyms t))

hasNoTypeSynonyms t =
  case t of
    TypeBasic _     -> True
    TypeVar _       -> True
    TypeCon c ts    -> case getTypeDecl c of
                         Nothing -> and (map hasNoTypeSynonyms ts)
                         Just _  -> False
    TypeList t1     -> hasNoTypeSynonyms t1
    TypeUnit        -> True
    TypeTuple ts    -> and (map hasNoTypeSynonyms ts)
    TypeFun t1 t2   -> hasNoTypeSynonyms t1 && hasNoTypeSynonyms t2
    TypeForall v t1 -> hasNoTypeSynonyms t1
    otherwise       -> False



-- Check that prepare does everything it is supposed to do, in any way.

prop_prepare t =
  (prepare t == (replaceTypeSynonyms $ closure t))
  && (prepare t == (closure $ replaceTypeSynonyms t))



