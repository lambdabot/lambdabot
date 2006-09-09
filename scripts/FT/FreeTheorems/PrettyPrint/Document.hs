-- Copyright 2006, Sascha Boehme.




-- | This module contains a class used as an abstraction of various
--   pretty-printing libraries. It is used as a foundation to pretty-print
--   types, relations and theorems in any format.

module FreeTheorems.PrettyPrint.Document where



import FreeTheorems.Types



-- | An abstract document for a certain type.
--   Every document type has to implement the member functions which are used as
--   combinators to pretty-print types and theorems.

class Document a where
  -- | The empty document.
  empty :: a

  -- | Concatenates two documents horizontally without putting space between
  --   them.
  (<>) :: a -> a -> a

  -- | Concatenates two documents horizontally and puts a soft space between
  --   them. A soft space may differ from the underlying document structure. If
  --   it puts spaces automatically between objects, than a soft space should be
  --   an empty document, otherwise it should be a space character.
  --
  --   If the two documents do not fit on the same line, a linebreak is used
  --   instead of the soft space.
  (<.>) :: a -> a -> a

  -- | Concatenates two documents horizontally and puts a space between them.
  --
  --   If the two documents do not fit on the same line, a linebreak is used
  --   instead of the space.
  (<+>) :: a -> a -> a

  -- | Concatenates two documents vertically.
  (<$>) :: a -> a -> a


  infixr 6 <>, <.>, <+>
  infixr 5 <$>

  -- | Concatenates a list of documents and uses soft spaces to separate the
  --   documents.
  --
  --   If the concatenated document exceeds the line length, a line break is
  --   inserted at the correct position instead of a soft space.
  --
  --   An implementation is already provided.
  fillSoft :: [a] -> a
  fillSoft ds =
    case ds of
      []        -> empty
      [d]       -> d
      otherwise -> foldr1 (<.>) ds

  -- | Concatenates a list of documents with a soft space between every
  --   document.
  --
  --   If the concatenated document exceeds the line length, a line break is
  --   inserted at the correct position instead of a space.
  --
  --   An implementation is already provided.
  fillSep :: [a] -> a
  fillSep ds =
    case ds of
      []        -> empty
      [d]       -> d
      otherwise -> foldr1 (<+>) ds

  -- | Concatenates a list of documents either horizontally, if it fits on the
  --   page, or vertically otherwise.
  --   When concatenating the documents horizontally, a soft space is inserted
  --   between each document.
  catSoft :: [a] -> a

  -- | Creates a document where all lines start at least from the current
  --   column.
  align :: a -> a

  -- | Indents the document. The amount of indentation is left to the
  --   implementation.
  indent :: a -> a





  -- | Puts '(' and ')' around the document.
  parens :: a -> a

  -- | Puts '[' and ']' around the document.
  brackets :: a -> a

  -- | Puts '{' and '}' around the document.
  braces :: a -> a

  -- | Writes the list of documents as a tuple (d1,d2,...,dn).
  --
  --   An implementation is already provided.
  tupled :: [a] -> a
  tupled ds = let i = map (\d -> d <> docComma) (init ds)
              in  parens (fillSoft (i ++ [last ds]))

  -- | Writes an instantiation of a document by another document.
  instant :: a -> a -> a





  -- | The comma @,@.
  docComma :: a

  -- | The dot @.@ as used to terminate quantifications.
  docDot :: a

  -- | The colon @:@ used as data constructor for lists or as double colon in
  --   type notations.
  docColon :: a

  -- | The arrow @->@.
  docArrow :: a

  -- | The universal quantifier.
  docForall :: a

  -- | The universal quantifier in the FixModel.
  docForallFix :: a

  -- | The equals sign @=@.
  docEqual :: a

  -- | The is-element-of sign.
  docIn :: a

  -- | The conjunction sign.
  docConjunction :: a

  -- | The implication sign.
  docImplication :: a

  -- | The union sign used when building the union of two sets.
  docUnion :: a

  -- | The bottom @_|_@.
  docBottom :: a

  -- | The bar @|@ used to separate elements from their restrictions in sets.
  docBar :: a

  -- | The mathematical symbol for the set of closed types.
  docTypes :: a

  -- | The mathematical symbol for the set of relations over closed types.
  docRel :: a

  -- | The identity relation @id@.
  docId :: a

  -- | The lift relation @lift@.
  docLift :: a

  -- | The pointed lift relation.
  docLiftPointed :: a

  -- | The text \"strict\".
  docStrict :: a

  -- | The text \"strict and continuous\".
  docStrictAndContinuous :: a





  -- | Creates a document from a basic type.
  docBasicType :: BasicType -> a

  -- | Creates a document from a type constructor.
  docTypeConstructor :: TypeConstructor -> a

  -- | Creates a document from a data constructor.
  docDataConstructor :: TypeConstructor -> a

  -- | Creates a document from a type variable.
  docTypeVariable :: TypeVariable -> a

  -- | Creates a document from a type term variable.
  docTypeTermVariable :: TypeTermVariable -> a

  -- | Creates a document from a term variable.
  docTermVariable :: TermVariable -> a

  -- | Creates a document from a relation variable.
  docRelationVariable :: RelationVariable -> a

  -- | Creates a document from an integer value.
  docInt :: Int -> a




