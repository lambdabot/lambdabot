-- Copyright 2006, Sascha Boehme.




-- | This module 'Theorem' and connected types to formulate theorems which are
--   generated from Haskell types.

module FreeTheorems.Types.Theorem where



import FreeTheorems.Types.LanguageModel
import FreeTheorems.Types.Type
import FreeTheorems.Types.Relation



-- | Defines a structure to describe free theorems.

--   The structure of the theorem type is essentially tree-like. Leaves
--   consists of pairs of terms being elements of relations, and nodes are
--   different universal quantifiers as well as conjunctions and implications.

data Theorem
  = IsElementOf (Term, Term) Relation
        -- ^ Models, that a pair of terms is an element of a relation.

  | ForallPairs (TermVariable,TermVariable) Relation Theorem
        -- ^ Models a universal quantification over pairs of a relation.

  | ForallRelations RelationVariable [Restriction] Theorem
        -- ^ Models a universal quantification over pairs of closed type terms
        --   and relations over these pairs where the relation respects the
        --   given list of restrictions.

  | ForallFunctions TermVariable (TypeTermVariable, TypeTermVariable)
                    [Restriction] Theorem
        -- ^ Models a universal quantification over pairs of closed type terms
        --   and functions over these pairs where the function respects the
        --   given list of restrictions.

  | ForallElements TermVariable Type Theorem
        -- ^ Models a universal quantification over elements from a certain
        --   type.

  | Conjunction Theorem Theorem
        -- ^ Models a conjunction between two theorems.

  | Implication Theorem Theorem
        -- ^ Models an implication between two theorems.

  deriving Show



-- | Gives certain restrictions occurring in theorems.

data Restriction
  = IsStrict TermVariable
        -- ^ Requests that a function denoted by a term variable must be strict.

  | IsStrictAndContinuous RelationVariable
        -- ^ Requests that a relation denoted by a relation variable must be
        --   strict and continous.

  deriving Show





-- | Gives a means to describe unfolded relations, i.e. relations and
--   corresponding sets which specify explicitely the members of that relation.

data UnfoldedRelation = UnfoldedRelation Relation LanguageModel UnfoldedSet



-- | The sets of an unfolded lift relation.

data UnfoldedSet

        -- | The sets of a general unfolded lift relation. Each set consists of
        --   the elements to create a notation like:
        --
        -- > { (D x1 ... xn, D y1 ... yn) | (xi,yi) in Ri }
        --
        --   where the theorem may be omitted in the case of nullary data
        --   constructors.
  = UnfoldedLift
      [(DataConstructor, [TermVariable], [TermVariable], Maybe Theorem)]

        -- | The set of an unfolded list relation. It corresponds to the
        --   notation:
        --
        -- > { (x:xs, y:ys) | (x,y) in R and (xs,ys) in lift_[](R) }
  | UnfoldedLiftList
      ((TermVariable, TermVariable), (TermVariable, TermVariable))
      Theorem

        -- | The set of an unfolded tuple relation. It corresponds to the
        --   notation:
        --
        -- > { ((x1, ..., xn), (y1, ..., yn)) | (xi,yi) in Ri }
  | UnfoldedLiftTuple
      ([TermVariable], [TermVariable])
      Theorem

        -- | The set of an unfolded functional relation. It corresponds to the
        --   notation:
        --
        -- > { (f, g) | forall (x,y) in R1. (f x, g y) in R2 }
  | UnfoldedFunction
      (TermVariable, TermVariable)
      Theorem

        -- | The set of an unfolded relational abstraction. It corresponds to
        --   the notation:
        --
        -- > { (x, y) | forall T1,T2 in TYPES. forall R in Rel(T1,T2).
        -- >            (x_T1, y_T2) in F(R) }
  | UnfoldedForall
      (TermVariable, TermVariable)
      Theorem





-- | A Haskell data constructor.

type DataConstructor = String




