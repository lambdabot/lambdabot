-- | Hackish Haddock module.
module Lambdabot.Plugin.Haddock (theModule) where

import Lambdabot.Plugin

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString,pack,unpack)

type HaddockState = M.Map ByteString [ByteString]
type Haddock = ModuleT HaddockState LB

theModule = newModule
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
        (intercalate (", ") . map unpack)
        (M.lookup (stripPs (pack k)) m)

-- make \@index ($) work.
stripPs :: ByteString -> ByteString
stripPs = fst . P.spanEnd (==')') . snd . P.span (=='(')
