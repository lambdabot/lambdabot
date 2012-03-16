{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | Hackish Haddock module.
module Plugin.Haddock (theModule) where

import Plugin

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack,unpack)

plugin "Haddock"

type HaddockState = M.Map ByteString [ByteString]

instance Module HaddockModule where
    type ModuleState HaddockModule = HaddockState
    
    moduleCmds = return
        [ (command "index")
            { help = say "index <ident>. Returns the Haskell modules in which <ident> is defined"
            , process = \k -> do
                m <- readMS
                say $ maybe "bzzt"
                    (intercalate (", ") . map unpack)
                    (M.lookup (stripPs (pack k)) m)
            }
        ]
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just (readOnly readPacked)

-- make \@index ($) work.
stripPs :: ByteString -> ByteString
stripPs = fst . P.spanEnd (==')') . snd . P.span (=='(')
