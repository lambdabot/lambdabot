-- Copyright 2006, Sascha Boehme.




-- | This module defines functions to pretty-print types and theorems as text.

module FreeTheorems.PrettyPrint.AsText (
    printLanguageModel,
    printNamedType,
    printTheorem,
    printUnfoldedRelation,
    printTermVariable,
    printRelationVariable
) where



import FreeTheorems.Types
import FreeTheorems.PrettyPrint.Common
import FreeTheorems.PrettyPrint.Document
import qualified PPrint as P





-- | Renders a document. This helper function is used both to render type
--   documents and theorem documents.
--
--   The parameters to renderPretty influence the output by setting the ribbon
--   factor and the line length.

renderDoc :: PDoc -> String
renderDoc (P doc) = P.displayS (P.renderPretty 1.0 75 doc) ""



--------------------------------------------------------------------------------



-- | Pretty-prints a language model.

printLanguageModel :: LanguageModel -> String
printLanguageModel BasicModel = "BasicModel"
printLanguageModel FixModel   = "FixModel"



-- | Pretty-prints a named type.

printNamedType :: NamedType -> String
printNamedType = renderDoc . docNamedType



-- | Pretty-prints a theorem.

printTheorem :: Theorem -> String
printTheorem = renderDoc . docTheorem



-- | Pretty-prints a term variable.

printTermVariable :: TermVariable -> String
printTermVariable = renderDoc . docTermVariable



-- | Pretty-prints a relation variable.

printRelationVariable :: RelationVariable -> String
printRelationVariable = renderDoc . docRelationVariable



-- | Pretty-prints an expanded relation.

printUnfoldedRelation :: UnfoldedRelation -> String
printUnfoldedRelation = renderDoc . docUnfoldedRelation



--------------------------------------------------------------------------------



-- | Defines text documents as liftings of the document type used in the PPrint
--   library.

newtype PDoc = P P.Doc


-- Define some lift operators for the PDoc type, so that the following code is
-- then better readable.

lift d1 = P d1

lift1 op (P d1) = P (op d1)

lift2 op (P d1) (P d2) = P (op d1 d2)

liftL op pds = P (op (map (\(P d) -> d) pds))

text = P . P.text



-- The instance declaration which specifies for every class method, how it
-- should work when pretty-printing objects as plain text. The following
-- definitions lift mainly some of the PPrint functions.

instance Document PDoc where

  -- The empty document.
  empty = lift P.empty

  -- Concatenates two documents horizontally without putting space between
  -- them.
  (<>) = lift2 (P.<>)

  -- Concatenates two documents horizontally and puts a soft space between
  -- them. A soft space may differ from the underlying document structure. If
  -- it puts spaces automatically between objects, than a soft space should be
  -- an empty document, otherwise it should be a space character.
  --
  -- If the two documents do not fit on the same line, a linebreak is used
  -- instead of the soft space.
  (<.>) = lift2 (P.</>)

  -- Concatenates two documents horizontally and puts a space between them.
  --
  -- If the two documents do not fit on the same line, a linebreak is used
  -- instead of the soft space.
  (<+>) = lift2 (P.</>)

  -- Concatenates two documents vertically.
  (<$>) = lift2 (P.<$>)

  -- Concatenates a list of documents either horizontally, if it fits on the
  -- page, or vertically otherwise.
  -- When concatenating the documents horizontally, a soft space is inserted
  -- between each document.
  catSoft = liftL P.sep

  -- Creates a document where all lines start at least from the current
  -- column.
  align = lift1 P.align

  -- Indents the document. The amount of indentation is left to the
  -- implementation.
  indent = lift1 (P.indent 2)




  -- Puts '(' and ')' around the document.
  parens = lift1 P.parens

  -- Puts '[' and ']' around the document.
  brackets = lift1 P.brackets

  -- Puts '{' and '}' around the document.
  braces = lift1 P.braces

  -- Writes an instantiation of a document by another document.
  instant d1 d2 = d1 <> (text "_") <> braces d2




  -- The comma @,@.
  docComma = text ","

  -- The dot @.@ as used to terminate quantifications.
  docDot = text "."

  -- The colon @:@ used as data constructor for lists or as double colon in
  -- type notations.
  docColon = text ":"

  -- The arrow @->@.
  docArrow = text "->"

  -- The universal quantifier.
  docForall = text "forall"

  -- The universal quantifier in the FixModel.
  docForallFix = text "forall-fix"

  -- The equals sign @=@.
  docEqual = text "="

  -- The is-element-of sign.
  docIn = text "in"

  -- The conjunction sign.
  docConjunction = text "/\\"

  -- The implication sign.
  docImplication = text "==>"

  -- The union sign used when building the union of two sets.
  docUnion = text "u"

  -- The bottom @_|_@.
  docBottom = text "_|_"

  -- The bar @|@ used to separate elements from their restrictions in sets.
  docBar = text "|"

  -- The mathematical symbol for the set of closed types.
  docTypes = text "TYPES"

  -- The mathematical symbol for the set of relations over closed types.
  docRel = text "REL"

  -- The identity relation @id@.
  docId = text "id"

  -- The lift relation @lift@.
  docLift = text "lift"

  -- The pointed lift relation.
  docLiftPointed = text "lift-pointed"

  -- The text \"strict\".
  docStrict = text "strict"

  -- The text \"strict and continuous\".
  docStrictAndContinuous = fillSep [text "strict", text "and",
                                    text "continuous"]




  -- Creates a document from a basic type.
  docBasicType b = text (show b)

  -- Creates a document from a type constructor.
  docTypeConstructor = text

  -- Creates a document from a data constructor.
  docDataConstructor = text

  -- Creates a document from a type variable.
  docTypeVariable = text

  -- Creates a document from a type term variable.
  docTypeTermVariable (T i) = text "T" <> docInt i

  -- Creates a document from a term variable.
  docTermVariable (PV tv)   = text tv
  docTermVariable (TV tv i) = text tv <> docInt i

  -- Creates a document from a relation variable.
  docRelationVariable (R i _ _)  = text "R" <> docInt i

  -- Creates a document from an integer value.
  docInt i = text (show i)


