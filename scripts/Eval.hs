--
-- Helper code for runplugs
--
module Eval where

import Language.Haskell.TH
import System.IO.Unsafe

instance Ppr a => Show (Q a) where
    show e = unsafePerformIO $ runQ e >>= return . pprint
