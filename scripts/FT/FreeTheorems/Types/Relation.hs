-- Copyright 2006, Sascha Boehme.




-- | This module declares 'Relation' and connected types to formulate relations
--   which are occurring in theorems.

module FreeTheorems.Types.Relation where



import FreeTheorems.Types.LanguageModel
import FreeTheorems.Types.Type



-- | A structure to describe relations occurring in free theorems.

data Relation
  = RelTerm Term (Type, Type)
        -- ^ A function as a special case of a relation.
        --   The additional pair stores the type of the function, i.e. if
        --   the function has type T1 -> T2, then the pair is (T1, T2).

  | RelVar RelationVariable
        -- ^ A relation variable.

  | RelLift LanguageModel TypeConstructor [Relation]
        -- ^ A lift relation for a certain type constructor.

  | RelLiftList LanguageModel Relation
        -- ^ A lift relation for lists over a given relation.

  | RelLiftTuple LanguageModel [Relation]
        -- ^ A lift relation for tuples of relations.

  | RelFun LanguageModel Relation Relation
        -- ^ A functional relation between two relations.

  | RelForall LanguageModel RelationVariable Relation
        -- ^ A relational abstraction.

  deriving (Eq, Show)



-- | Specifies relation variable or function variables as a special case of a
--   relation.

--   This definition is close to the mathematical notation @R_i@ in LaTeX.
--   To every relation variable a number is assigned which is unique among
--   relation variables.

data RelationVariable = R Int TypeVariable (TypeTermVariable, TypeTermVariable)
                            -- ^ A relation variable with a unique index.
                        deriving (Eq, Show)



-- | Specifies terms which could occur in pairs of free theorems. Terms can
--   either be just variables, applications of variables to terms or
--   instantiations of terms.

data Term
  = TermVar TermVariable    -- ^ A variable.

  | TermApp Term Term       -- ^ An application of a term to a term.

  | TermIns Term Type       -- ^ An instantiation of a term by a closed type.
  deriving (Eq, Show)



