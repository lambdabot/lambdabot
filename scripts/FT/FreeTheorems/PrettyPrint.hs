-- Copyright 2006, Sascha Boehme.




-- | This module contains classes to pretty-print types and theorems in
--   different formats.

module FreeTheorems.PrettyPrint where



-- This module is just an interface for all pretty-print modules. The intention
-- is that this module hides the implementation of several output formats by a
-- simple interface. For each output format, there should be one module in the
-- 'PrettyPrint' subdirectory (according to the 'AsText' module) and also one
-- class similar to 'PrettyPrintAsText'.



import FreeTheorems.Types
import qualified FreeTheorems.PrettyPrint.AsText as Text





-- | Conversion of values to 'String's.
--   Derived instances of 'PrettyPrintAsText' show their values using
--   'printAsText'. The pretty-printed text is optimized consoles or text files
--   having a line length of at least 75 characters.

class PrettyPrintAsText a where
  -- | Pretty-prints a value as plain text with a maximum line length of 75
  --   characters.
  printAsText :: a -> String



instance PrettyPrintAsText LanguageModel where
  printAsText = Text.printLanguageModel



instance PrettyPrintAsText NamedType where
  printAsText = Text.printNamedType



instance PrettyPrintAsText Theorem where
  printAsText = Text.printTheorem



instance PrettyPrintAsText UnfoldedRelation where
  printAsText = Text.printUnfoldedRelation



instance PrettyPrintAsText TermVariable where
  printAsText = Text.printTermVariable



instance PrettyPrintAsText RelationVariable where
  printAsText = Text.printRelationVariable


