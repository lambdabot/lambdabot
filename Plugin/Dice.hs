{-# LANGUAGE TemplateHaskell #-}
-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)

-- Original version copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
-- Massive rewrite circa 2008-10-20 copyright James Cook <mokus@deepbondi.net>
module Plugin.Dice (theModule) where

import Plugin

import Message

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
    moduleCmds   _               = ["dice", "roll"]
    moduleHelp   _ cmd           = cmd ++ " <expr>. Throw random dice. <expr> is of the form 3d6+2."
    process      _ msg _ _  text = doDice msg text True
    contextual   _ msg _    text = doDice msg text False


----------------------------------------------------------------
-- the IRC shim stuff

doDice :: Message a => a -> String -> Bool -> ModuleLB ()
doDice msg text printErrs = do
    result <- io (rollEm text)
    return $ case result of
        Left err    -> if printErrs
            then [trimError err]
            else []
        Right str   -> 
            [brk 75 (user ++ ": " ++ str)]
    
    where
        user = nName $ nick $ msg
        trimError = concat . intersperse ": " . tail . lines . show
        brk n s | length s <= n = s
                | otherwise     = take (n-3) s ++ "..."
