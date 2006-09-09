-- Copyright 2006, Sascha Boehme.




-- | This module declares 'NamedType', 'Type' and related types to describe the
--   structure of Haskell types.

module FreeTheorems.Types.Type where





-- | A Haskell type with an associated name.

data NamedType = NamedType TermVariable Type
                 deriving (Eq, Show)


-- | Describes Haskell types.
--   This algebraic datatype is powerful enough to not only model Haskell98
--   types, but also higher-rank function types.

data Type
  = TypeBasic BasicType         -- ^ A basic type.

  | TypeVar TypeVariable        -- ^ A type variable.

  | TypeTermVar TypeTermVariable    -- ^ A type term variable representing an
                                    --   abstract type term.

  | TypeCon TypeConstructor [Type]
                                -- ^ An algebraic datatype with type arguments.

  | TypeList Type               -- ^ The list type.

  | TypeUnit                    -- ^ The unit type @()@.

  | TypeTuple [Type]            -- ^ Tuple types. Although this construct allows
                                --   arbitrary typle sizes, tuples are generally
                                --   restricted to a size between 2 and 15.

  | TypeFun Type Type           -- ^ A function type of the two argument types.

  | TypeForall TypeVariable Type
                                -- ^ A @forall@ type abstraction.

  deriving (Eq, Show)



-- | Defines the basic types of Haskell. The data constructors use the same
--   names as the corresponding Haskell types.

data BasicType = Char | Int | Integer | Float | Double
                 deriving (Eq, Show)



-- | Defines a type to represent type constructors.

type TypeConstructor = String



-- | Describes Haskell type variables.

type TypeVariable = String



-- | Describes a variable occurring in terms.

data TermVariable
  = TV String Int   -- ^ An automatically generated term variable where the
                    --   string is a prefix (like @x@) and the integer is an
                    --   index.

  | PV String       -- ^ A parsed term variable, usually the name of a type.

  deriving (Eq, Show)



-- | Defines a type term variable.

--   This definition is close to the mathematical notation @T_i@ in LaTeX.
--   To every relation variable a number is assigned which is unique among
--   relation variables.

newtype TypeTermVariable = T Int    -- ^ A type term variable with an index.
                           deriving (Eq, Show)
