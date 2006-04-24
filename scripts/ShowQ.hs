--
-- Helper code for runplugs
--
-- Note: must be kept in separate module to hide unsafePerformIO from
-- runplugs-generated eval code!! you're warned.
--
module ShowQ where

import Language.Haskell.TH
import System.IO.Unsafe
import Data.Dynamic

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

instance Ppr a => Show (Q a) where
    show e = unsafePerformIO $ runQ e >>= return . pprint
