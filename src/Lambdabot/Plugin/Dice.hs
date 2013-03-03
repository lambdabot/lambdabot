-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)

-- Original version copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
-- Massive rewrite circa 2008-10-20 copyright James Cook <mokus@deepbondi.net>
module Lambdabot.Plugin.Dice (theModule) where

import Lambdabot.Plugin

import Data.List
import Data.Random.Dice (rollEm)

type Dice = ModuleT () LB

theModule :: Module ()
theModule = newModule
    { moduleCmds = return
        [ (command "dice")
            { aliases = ["roll"]
            , help = say "@dice <expr>. Throw random dice. <expr> is of the form 3d6+2."
            , process = doDice True
            }
        ]
    , contextual = doDice False
    }

doDice :: Bool -> String -> Cmd Dice ()
doDice printErrs text = do
    user <- showNick =<< getSender
    result <- io (rollEm text)
    case result of
        Left err    -> if printErrs
            then say (trimError err)
            else return ()
        Right str   -> 
            say (limitStr 75 (user ++ ": " ++ str))
    
    where
        trimError = concat . intersperse ": " . tail . lines . show
