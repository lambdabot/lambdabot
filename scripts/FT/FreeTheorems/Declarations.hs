-- Copyright 2006, Sascha Boehme.




-- | This module defines the accepted user-defined types.
--   It covers only algebraic datatypes and type synonyms.

module FreeTheorems.Declarations (

    -- * Haskell type declarations
    DataDeclaration(..),
    DataConstructorDeclaration(..),
    Type(..),
    TypeDeclaration(..),
    TypeConstructor,
    Arity,

    -- * Type information
    isDefined,
    getDataDecl,
    getTypeDecl,
    getSupportedAlgebraicDatatypes,
    getSupportedTypeSynonyms,

    -- testing interface
    testDeclarations
) where



import FreeTheorems.Types



-- | Gives a notation for the declaration of an algebraic datatype.
--   The right-hand side is defined as a function type to replace type variables
--   easily by arbitrary types. The argument list of this function should be
--   exactly as long as the arity of the type constructor.
--   The types of the right-hand side should not contain any type synonym
--   because right-hand sides will not be prepared the same way as types
--   inputted by the user.

data DataDeclaration = DataDecl TypeConstructor Arity
                                ([Type] -> [DataConstructorDeclaration])



-- | Denotes the declaration of a data constructor as it occurs in definitions
--   of algebraic datatypes.

data DataConstructorDeclaration = DataConDecl DataConstructor [Type]



-- | Represents the declaration of a type synonym.
--   The right-hand side is defined as a function type to replace type variables
--   easily by arbitrary types. The argument list of this function should be
--   exactly as long as the arity of the type constructor.

data TypeDeclaration = TypeDecl TypeConstructor Arity ([Type] -> Type)



-- | Used to describe the rank or arity of type constructors.

type Arity = Int



-- | Stores valid user-defined types.
--   It contains algebraic datatypes and type synonyms along with their
--   right-hand side.

data UserDefinedTypes = UserDefinedTypes {
  dataDecls :: [DataDeclaration],   -- ^ A list of valid algebraic data types
                                    --   each of them with their right-hand
                                    --   side.

  typeDecls :: [TypeDeclaration]    -- ^ A list of valid type synonymes each of
                                    --   them with their right-hand side.
}



-- | Checks if a type constructor occurs in the list of accepted user-defined
--   types.

isDefined :: TypeConstructor -> Bool
isDefined t =
     any (\(DataDecl t' _ _) -> (t == t')) (dataDecls userDefinedTypes)
  || any (\(TypeDecl t' _ _) -> (t == t')) (typeDecls userDefinedTypes)



-- | Returns the declaration of an algebraic datatype, if it occurs in the list
--   of accepted algebraic datatypes.

getDataDecl :: TypeConstructor -> Maybe DataDeclaration
getDataDecl t =
  case (filter (\(DataDecl t' _ _)-> (t == t')) (dataDecls userDefinedTypes)) of
    []    -> Nothing
    (d:_) -> Just d



-- | Returns the declaration of a type synonym, if it occurs in the list of
--   accepted type synonyms.

getTypeDecl :: TypeConstructor -> Maybe TypeDeclaration
getTypeDecl t =
  case (filter (\(TypeDecl t' _ _)-> (t == t')) (typeDecls userDefinedTypes)) of
    []    -> Nothing
    (d:_) -> Just d



-- | Returns the list of algebraic datatypes being supported by the FreeTheorems
--   library. For each algebraic datatype, its name and arity are returned.

getSupportedAlgebraicDatatypes :: [(String, Int)]
getSupportedAlgebraicDatatypes = map (\(DataDecl n i _) -> (n,i))
                                     (dataDecls userDefinedTypes)



-- | Returns the list of type synonyms being supported by the FreeTheorems
--   library. For each type synonym, its name and arity are returned.

getSupportedTypeSynonyms :: [(String, Int)]
getSupportedTypeSynonyms = map (\(TypeDecl n i _) -> (n,i))
                               (typeDecls userDefinedTypes)



-- | Defines the set of valid user-defined types.
--   This sets defines all types which may occur in type terms from which
--   theorems can be generated. Except of predefined types, no more types are
--   allowed in type terms.

userDefinedTypes :: UserDefinedTypes
userDefinedTypes = UserDefinedTypes {

-- Note that the types of the right-hand side should not contain any type
-- synonym because it will not be replaced.
  dataDecls = [
    DataDecl "Bool" 0 (\[] ->
             [DataConDecl "False" []
             ,DataConDecl "True" []
             ]),
    DataDecl "Maybe" 1 (\[a] ->
             [DataConDecl "Nothing" []
             ,DataConDecl "Just" [a]
             ]),
    DataDecl "Either" 2 (\[a, b] ->
             [DataConDecl "Left" [a]
             ,DataConDecl "Right" [b]
             ]),
    DataDecl "Ordering" 0 (\[] ->
             [DataConDecl "LT" []
             ,DataConDecl "EQ" []
             ,DataConDecl "GT" []
             ]),
    DataDecl "IOMode" 0 (\[] ->
             [DataConDecl "ReadMode" []
             ,DataConDecl "WriteMode" []
             ,DataConDecl "AppendMode" []
             ,DataConDecl "ReadWriteMode" []
             ]),
    DataDecl "BufferMode" 0 (\[] ->
             [DataConDecl "NoBuffering" []
             ,DataConDecl "LineBuffering" []
             ,DataConDecl "BlockBuffering" [TypeCon "Maybe" [TypeBasic Int]]
             ]),
    DataDecl "SeekMode" 0 (\[] ->
             [DataConDecl "AbsoluteSeek" []
             ,DataConDecl "RelativeSeek" []
             ,DataConDecl "SeekFromEnd" []
             ]),
    DataDecl "Permissions" 0 (\[] ->
             [DataConDecl "Permissions" [TypeCon "Bool" []  -- readable
                                        ,TypeCon "Bool" []  -- writable
                                        ,TypeCon "Bool" []  -- executable
                                        ,TypeCon "Bool" []  -- searchable
                                        ]]),
    DataDecl "ExitCode" 0 (\[] ->
             [DataConDecl "ExitSuccess" []
             ,DataConDecl "ExitFailure" [TypeBasic Int]
             ]),
    DataDecl "Month" 0 (\[] ->
             [DataConDecl "January" []
             ,DataConDecl "February" []
             ,DataConDecl "March" []
             ,DataConDecl "April" []
             ,DataConDecl "May" []
             ,DataConDecl "June" []
             ,DataConDecl "July" []
             ,DataConDecl "August" []
             ,DataConDecl "September" []
             ,DataConDecl "October" []
             ,DataConDecl "November" []
             ,DataConDecl "December" []
             ]),
    DataDecl "Day" 0 (\[] ->
             [DataConDecl "Sunday" []
             ,DataConDecl "Monday" []
             ,DataConDecl "Tuesday" []
             ,DataConDecl "Wednesday" []
             ,DataConDecl "Thursday" []
             ,DataConDecl "Friday" []
             ,DataConDecl "Saturday" []
             ]),
    DataDecl "CalenderTime" 0 (\[] ->
             [DataConDecl "CalenderTime" [TypeBasic Int     -- ctYear
                                         ,TypeCon "Month" []    -- ctMonth
                                         ,TypeBasic Int     -- ctDay
                                         ,TypeBasic Int     -- ctHour
                                         ,TypeBasic Int     -- ctMin
                                         ,TypeBasic Int     -- ctSec
                                         ,TypeBasic Integer -- ctPicosec
                                         ,TypeCon "Day" []  -- ctWDay
                                         ,TypeBasic Int     -- ctYDay
                                         ,TypeList (TypeBasic Char)  -- ctTZName
                                         ,TypeBasic Int     -- ctTZ
                                         ,TypeCon "Bool" [] -- ctIsDST
                                         ]]),
    DataDecl "TimeDiff" 0 (\[] ->
             [DataConDecl "TimeDiff" [TypeBasic Int     -- tdYear
                                     ,TypeBasic Int     -- tdMonth
                                     ,TypeBasic Int     -- tdDay
                                     ,TypeBasic Int     -- tdHour
                                     ,TypeBasic Int     -- tdMin
                                     ,TypeBasic Int     -- tdSec
                                     ,TypeBasic Integer -- tdPicosec
                                     ]]),
    DataDecl "TimeLocale" 0 (\[] ->
             [DataConDecl "TimeLocale"
                [TypeList (TypeTuple [TypeList (TypeBasic Char)
                                     ,TypeList (TypeBasic Char)]) -- wDays
                ,TypeList (TypeTuple [TypeList (TypeBasic Char)
                                     ,TypeList (TypeBasic Char)]) -- months
                ,TypeTuple [TypeList (TypeBasic Char)
                           ,TypeList (TypeBasic Char)]            -- amPm
                ,TypeList (TypeBasic Char)        -- dateTimeFmt
                ,TypeList (TypeBasic Char)        -- dateFmt
                ,TypeList (TypeBasic Char)        -- timeFmt
                ,TypeList (TypeBasic Char)        -- time12Fmt
                ]])
  ],

  typeDecls = [
    TypeDecl "String" 0
             (\[] -> TypeList (TypeBasic Char)),
    TypeDecl "ReadS" 1
             (\[a] -> TypeFun (TypeCon "String" [])
                              (TypeList (TypeTuple [a, TypeCon "String" []]))),
    TypeDecl "ShowS" 0
             (\[] -> TypeFun (TypeCon "String" []) (TypeCon "String" [])),
    TypeDecl "FilePath" 0
             (\[] -> TypeCon "String" [])
  ]
}



--------------------------------------------------------------------------------



-- A list of tests for this module.

testDeclarations = do
  putStr "declarations contain no function and forall ... "
  putStrLn (if test_no_fun_forall then "OK" else "FALSE")



-- Check that data declarations contain no functions and type abstractions in
-- its right-hand sides.

test_no_fun_forall = and $ map hasNoFunForall (dataDecls userDefinedTypes)

hasNoFunForall (DataDecl _ n f) =
  let vs = map (\i -> TypeVar ("a" ++ show i)) [1..n]
      ts = concatMap (\(DataConDecl _ ts) -> ts) (f vs)
  in  and $ map noFunForall ts

noFunForall t =
  case t of
    TypeBasic _    -> True
    TypeVar _      -> True
    TypeTermVar _  -> True
    TypeCon _ ts   -> and $ map noFunForall ts
    TypeList t'    -> noFunForall t'
    TypeUnit       -> True
    TypeTuple ts   -> and $ map noFunForall ts
    TypeFun _ _    -> False
    TypeForall _ _ -> False
