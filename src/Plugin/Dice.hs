{-# LANGUAGE TemplateHaskell #-}
-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)

-- Original version copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
-- Massive rewrite circa 2008-10-20 copyright James Cook <mokus@deepbondi.net>
module Plugin.Dice (theModule) where

import Plugin

import Data.Random.Dice (rollEm)

$(plugin "Dice")

instance Module DiceModule where
    moduleCmds _ =
        [ (command "dice")
            { aliases = ["roll"]
            , help = say "@dice <expr>. Throw random dice. <expr> is of the form 3d6+2."
            , process = doDice True
            }
        ]
    contextual _ = doDice False

----------------------------------------------------------------
-- the IRC shim stuff

doDice :: MonadIO m => Bool -> String -> Cmd m ()
doDice printErrs text = do
    user <- showNick =<< getSender
    result <- io (rollEm text)
    case result of
        Left err    -> if printErrs
            then say (trimError err)
            else return ()
        Right str   -> 
            say (brk 75 (user ++ ": " ++ str))
    
    where
        trimError = concat . intersperse ": " . tail . lines . show
        brk n s | length s <= n = s
                | otherwise     = take (n-3) s ++ "..."
