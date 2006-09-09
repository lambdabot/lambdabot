-- Copyright 2006, Sascha Boehme.




-- | This module is the interface to the FreeTheorems library. It exposes all
--   types and functions needed to parse types, generate theorems for them and
--   pretty-print types and theorems.
--
--   Although every type below is an instance of the class 'Show', the provided
--   pretty-printing classes should be used to display them. The standard 'show'
--   function is only available for debugging purposes.

module FreeTheorems (

-- * Types and theorems
    LanguageModel(..),
    NamedType(..),
    TermVariable,
    Type,
    Theorem,
    RelationVariable,
    UnfoldedRelation,
    TheoremData,

-- * Parsing of types
    parseNamedType,
    parseType,
    getSupportedAlgebraicDatatypes,
    getSupportedTypeSynonyms,

-- * Generation and deduction of theorems
    generateTheorem,
    extractLiftRelations,
    Specialization.extractRelationVariables,
    instantiateRelation,
    omitInstantiations,

-- * Pretty-printing
    PrettyPrintAsText(..)
) where



import FreeTheorems.Declarations
import FreeTheorems.Delta
import qualified FreeTheorems.Lifts as Lifts
import FreeTheorems.Modification
import FreeTheorems.PrettyPrint
import qualified FreeTheorems.Specialization as Specialization
import FreeTheorems.TheoremData
import FreeTheorems.TypeParser
import FreeTheorems.Types
import FreeTheorems.Unfolding



-- | Generates a theorem for a named type using a certain language model.

generateTheorem :: LanguageModel -> NamedType -> (Theorem, TheoremData)
generateTheorem model (NamedType tv t) =
  execute tv (applyDelta model t >>= unfold tv)



-- | Creates a list of all lifted relations occurring in the given theorem.
--   Lifted relations are @lift@ relations as well as functional relations and
--   relational abstractions.
--   The lifted relations are returned together with their unfolded description.

extractLiftRelations :: (Theorem, TheoremData) -> Bool -> [UnfoldedRelation]
extractLiftRelations (theorem, tdata) omitIns =
  fst $ executeWith tdata (Lifts.extractLiftRelations theorem omitIns)



-- | Instantiates a relation variable found by
--   'Specialization.extractRelationVariables' in the given theorem.
--   A new function symbol is created to replace the relation variable.

instantiateRelation :: (Theorem, TheoremData)
                       -> RelationVariable
                       -> (Theorem, TheoremData)

instantiateRelation (theorem, tdata) rv =
  executeWith tdata (Specialization.replaceRelationVariable theorem rv)
