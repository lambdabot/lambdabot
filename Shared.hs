{-# LANGUAGE Rank2Types, TypeOperators #-}

-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Shared types between static and dynamic code. This is the only
-- module that is linked both statically and dynamically. (And doing so
-- breaks us from running the whole of the bot in ghci -- so sue me).

module Shared (Module(..), Symbol, DynLoad(..)) where

type Symbol = String

newtype Module = Module String {- unique module identifier -}

-- | Operations provided by the dynamic linker linked into the static
-- core. A DynLoad value is passed from there to the dynamic code, for
-- use by DynamicModule
--
-- Possibly want to treat this as an existential, ala MODULE
--
-- could add to DynLoad, Typeable a => ...
--
data DynLoad = DynLoad {
        dynload    :: forall a. FilePath -> Symbol -> IO (Module,a),
        unload     :: Module   -> IO ()
    }

