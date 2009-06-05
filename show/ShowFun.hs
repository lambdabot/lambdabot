-- Helper code for runplugs that doesn't agree with SmallCheck

module ShowFun where

import Data.Typeable

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

instance Typeable a => Show (IO a) where
    show e = '<' : (show . typeOf) e ++ ">"
