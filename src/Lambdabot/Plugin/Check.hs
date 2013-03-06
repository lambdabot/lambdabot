-- Copyright (c) 6 DonStewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Test a property with QuickCheck
module Lambdabot.Plugin.Check (theModule) where

import Lambdabot.Plugin
import Lambdabot.Plugin.Eval (eval)
import qualified Language.Haskell.Exts as Hs

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command "check")
            { help = do
                say "check <expr>"
                say "You have QuickCheck and 3 seconds. Prove something."
            , process = lim80 . check
            }
        ]
    }

check :: MonadLB m => String -> m String
check src =
    case Hs.parseExp src of
        Hs.ParseFailed l e  -> return (Hs.prettyPrint l ++ ':' : e)
        Hs.ParseOk{}        -> eval ("myquickcheck (" ++ src ++ ") `seq` hsep[]")
