{-# LANGUAGE TemplateHaskell #-}
-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)

-- Original version copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
-- Massive rewrite circa 2008-10-20 copyright James Cook <mokus@deepbondi.net>
module Plugin.Dice (theModule) where

import Plugin

import Lambdabot.Message (nick, nName)

import Control.Monad                    (replicateM,foldM)
import System.Random                    (Random, randomRIO)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Data.Ratio
import Data.Random.Dice (rollEm)
import Text.Printf

$(plugin "Dice")

instance Module DiceModule where
    moduleCmds _ =
        [ (command "dice")
            { aliases = ["roll"]
            , help = say "@dice <expr>. Throw random dice. <expr> is of the form 3d6+2."
            , process = \args -> do
                user <- showNick =<< getSender
                doDice True user args >>= mapM_ say
            }
        ]
    contextual _ msg _ = doDice False (nName $ nick msg)

----------------------------------------------------------------
-- the IRC shim stuff

doDice :: MonadIO m => Bool -> String -> String -> m [String]
doDice printErrs user text = do
    result <- io (rollEm text)
    return $ case result of
        Left err    -> if printErrs
            then [trimError err]
            else []
        Right str   -> 
            [brk 75 (user ++ ": " ++ str)]
    
    where
        trimError = concat . intersperse ": " . tail . lines . show
        brk n s | length s <= n = s
                | otherwise     = take (n-3) s ++ "..."
