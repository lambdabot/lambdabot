--
-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)
--
-- Copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
--
module Plugin.Dice (theModule) where

import Plugin

import Data.Char                        (isSpace)
import Control.Monad                    (replicateM,foldM)
import Control.Monad.Trans              (liftIO)
import System.Random                    (randomRIO)
import Text.ParserCombinators.Parsec

newtype DiceModule = DiceModule ()

theModule :: MODULE
theModule = MODULE $ DiceModule ()

instance Module DiceModule () where
    moduleCmds   _  = ["dice"]
    moduleHelp _ _  = "dice <expr>. Throw random dice. <expr> is of the form 3d6+2."
    process_ _ _ xs = liftIO (dice xs) >>= return . (:[])

dice :: String -> IO String
dice str = case parse expr "dice" (filter (not.isSpace) str) of
            Left err  -> return $ show err
            Right e   -> do res <- eval e
                            return (str++" => "++show res)

eval :: [(Int, Int)] -> IO Int
eval = foldM ef 0
    where ef acc (v,1) = return (acc+v)
          ef acc (n,d) = if n > 100
                            then return 0
                            else do list <- replicateM n (randomRIO (1,d))
                                    return (acc + sum list)


expr :: CharParser st [(Int, Int)]
expr = primExp `sepBy1` (char '+')

primExp :: CharParser st (Int, Int)
primExp = do v <- number
             d <- option 1 (char 'd' >> number)
             return (v,d)

number :: CharParser st Int
number = read `fmap` many1 digit

