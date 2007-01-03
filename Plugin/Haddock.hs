--
-- | Hackish Haddock module.
--
module Plugin.Haddock (theModule) where

import Plugin

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString (ByteString)

PLUGIN Haddock

type HaddockState = M.Map ByteString [ByteString]

instance Module HaddockModule HaddockState where
    moduleCmds      _ = ["index"]
    moduleHelp    _ _ = "index <ident>. Returns the Haskell modules in which <ident> is defined"
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just (readOnly readPacked)

    process_ _ _ rest = do
        assocs <- readMS
        box $ maybe "bzzt" (concatWith ", " . (map P.unpack))
                           (M.lookup (P.pack (stripParens rest)) assocs)

-- | make \@index ($) work.
stripParens :: String -> String
stripParens = reverse . dropWhile (==')') . reverse . dropWhile (=='(')

