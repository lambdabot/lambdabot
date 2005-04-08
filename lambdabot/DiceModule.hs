--
-- This module is for throwing dice for e.g. RPGs. (@dice 3d6+2)
-- Copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
--
module DiceModule (theModule) where

import IRC

import Data.Char                        (isSpace)
import Control.Monad                    (replicateM,foldM)
import Control.Monad.Trans              (liftIO)
import System.Random                    (randomRIO)
import Text.ParserCombinators.Parsec

newtype DiceModule = DiceModule ()

theModule :: MODULE
theModule = MODULE diceModule

diceModule :: DiceModule
diceModule = DiceModule ()

instance Module DiceModule () where
  moduleName   _ = return "dice"
  moduleSticky _ = False

  moduleHelp _ s = return $ case s of
        "dice"       -> "@dice <expr>. Throw dice. <expr> of the form 3d6+2."
        _             -> "dice module"

  commands     _ = return ["dice"]
  process _ _ src "dice" rest = ircPrivmsg src =<< liftIO (dice rest)
  process _ _ _    _     _    = error "Dice: invalid command"

dice :: String -> IO String
dice str = case parse expr "dice" (filter (not.isSpace) str) of
            Left err  -> return $ show err
            Right e   -> do res <- eval e
                            return (str++" => "++show res)

eval :: [(Int, Int)] -> IO Int
eval = foldM ef 0
    where ef acc (v,1) = return (acc+v)
          ef acc (n,d) = do list <- replicateM n (randomRIO (1,d))
                            return (acc + sum list)

expr :: CharParser st [(Int, Int)]
expr = primExp `sepBy1` (char '+')

primExp :: CharParser st (Int, Int)
primExp = do v <- number
             d <- option 1 (char 'd' >> number)
             return (v,d)

number :: CharParser st Int
number = many1 digit >>= (return . read)

