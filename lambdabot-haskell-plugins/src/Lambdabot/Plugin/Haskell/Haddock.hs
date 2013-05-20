-- | Hackish Haddock module.
module Lambdabot.Plugin.Haskell.Haddock (haddockPlugin) where

import Lambdabot.Plugin

import qualified Data.ByteString.Char8 as P
import Data.List
import qualified Data.Map as M

type HaddockState = M.Map P.ByteString [P.ByteString]
type Haddock = ModuleT HaddockState LB

haddockPlugin :: Module HaddockState
haddockPlugin = newModule
    { moduleCmds = return
        [ (command "index")
            { help = say "index <ident>. Returns the Haskell modules in which <ident> is defined"
            , process = doHaddock
            }
        ]
        
    , moduleDefState  = return M.empty
    , moduleSerialize = Just (readOnly readPacked)
    }

doHaddock :: String -> Cmd Haddock ()
doHaddock k = do
    m <- readMS
    say $ maybe "bzzt"
        (intercalate (", ") . map P.unpack)
        (M.lookup (stripPs (P.pack k)) m)

-- make \@index ($) work.
stripPs :: P.ByteString -> P.ByteString
stripPs = fst . P.spanEnd (==')') . snd . P.span (=='(')
