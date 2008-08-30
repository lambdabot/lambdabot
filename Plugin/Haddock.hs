{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Hackish Haddock module.
module Plugin.Haddock (theModule) where

import Plugin

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack)

$(plugin "Haddock")

type HaddockState = M.Map ByteString [ByteString]

instance Module HaddockModule HaddockState where
    moduleCmds      _ = ["index"]
    moduleHelp    _ _ = "index <ident>. Returns the Haskell modules in which <ident> is defined"
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just (readOnly readPacked)
    fprocess_ _ _ k = readMS >>= \m -> box $ maybe
        (pack "bzzt")
        (P.intercalate (pack ", "))
        (M.lookup (stripPs k) m)

        where
          -- make \@index ($) work.
          stripPs :: ByteString -> ByteString
          stripPs = fst . P.spanEnd (==')') . snd . P.span (=='(')

