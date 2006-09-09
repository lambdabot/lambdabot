-- Copyright 2006, Sascha Boehme.




-- | This module declares 'LanguageModel' which gives means to restrict the
--   covered language subset of Haskell when generating theorems,
--
module FreeTheorems.Types.LanguageModel where



-- | Specifies the Haskell language model used in generating theorems.

data LanguageModel
  = BasicModel      -- ^ Most restrictive language model which does not allow
                    --   any of the relaxations the other models are providing.

  | FixModel        -- ^ Allows fixpoints and _|_ in the used Haskell subset.

  deriving (Eq, Show)
