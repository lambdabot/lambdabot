--
-- | Hackish Haddock module.
--
module Plugin.Haddock (theModule) where

import Plugin

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P

PLUGIN Haddock

type HaddockState = M.Map P.ByteString [P.ByteString]

instance Module HaddockModule HaddockState where
    moduleCmds      _ = ["index"]
    moduleHelp    _ _ = "index <ident>. Returns the Haskell modules in which <ident> is defined"
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just $ Serial { deserialize = Just . readPacked
                                      , serialize   = const Nothing }
    process_ _ _ rest = do
       assocs <- readMS
       return . (:[]) $ maybe "bzzt" (concatWith ", " . (map P.unpack))
                                     (M.lookup (P.pack (stripParens rest)) assocs)

-- | make \@index ($) work.
stripParens :: String -> String
stripParens = reverse . dropWhile (==')') . reverse . dropWhile (=='(')

