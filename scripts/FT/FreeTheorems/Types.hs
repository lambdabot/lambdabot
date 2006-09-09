-- Copyright 2006, Sascha Boehme.




-- | This module contains declarations for all major types used in the
--   Free Theorems library. It includes the following types:
--
--       * 'LanguageModel' which gives means to restrict the covered language
--         subset of Haskell when generating theorems,
--
--       * 'NamedType', 'Type' and related types to describe the structure of
--         Haskell types,
--
--       * 'Theorem', 'Relation' and connected types to formulate theorems which
--         are generated from Haskell types.

module FreeTheorems.Types (
    -- * Language model
    module FreeTheorems.Types.LanguageModel,

    -- * Haskell types
    module FreeTheorems.Types.Type,

    -- * Relations
    module FreeTheorems.Types.Relation,

    -- * Theorems
    module FreeTheorems.Types.Theorem
) where



import FreeTheorems.Types.LanguageModel
import FreeTheorems.Types.Type
import FreeTheorems.Types.Relation
import FreeTheorems.Types.Theorem


