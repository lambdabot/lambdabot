{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | This module is for throwing dice for e.g. RPGs. (\@dice 3d6+2)

-- Copyright Einar Karttunen <ekarttun@cs.helsinki.fi> 2005-04-06.
module Plugin.Dice (theModule) where

import Plugin

import Control.Monad                    (replicateM,foldM)
import System.Random                    (randomRIO)
import Text.ParserCombinators.Parsec

$(plugin "Dice")

instance Module DiceModule () where
    moduleCmds   _  = ["dice"]
    moduleHelp _ _  = "dice <expr>. Throw random dice. <expr> is of the form 3d6+2."
    process_ _ _ xs = ios (dice xs)

dice :: String -> IO String
dice str = case parse expr "dice" (filter (not.isSpace) str) of
            Left err  -> return . trimError $ err
            Right e   -> do res <- eval e
                            return (brk 30 str++" => "++brk 45 (show res))
           where
            brk n s | length s <= n = s
                    | otherwise     = take (n-3) s ++ "..."
            trimError = concat . intersperse ": " . tail . lines . show

eval :: [(Integer, Integer)] -> IO Integer
eval = foldM ef 0
    where ef acc (v,1) = return (acc+v)
          ef acc (n,d) = if n > 100
                            then do x <- ndRandomIO
                                    let e = n*(d+1)`div`2
                                        e' = fromIntegral (n*(d+1)`mod`2)/2
                                        v = fromIntegral (d*d-1)/12
                                        x' = e' + x * sqrt (fromIntegral n * v)
                                    return (acc + e + round x')
                            else do ls <- replicateM (fromIntegral n)
                                                     (randomRIO (1,d))
                                    return (acc + sum ls)

-- | get a normally distributed random number
ndRandomIO :: IO Double
ndRandomIO = do r   <- randomRIO (0, 1)
                phi <- randomRIO (0, 2*pi)
                let r' = sqrt (-2 * log r)
                return (r' * sin phi)

expr :: CharParser st [(Integer, Integer)]
expr = do res <- primExp `sepBy1` (char '+')
          eof <?> "end"
          return res

primExp :: CharParser st (Integer, Integer)
primExp = do v <- number
             d <- option 1 (char 'd' >> number)
             return (v,d)

number :: CharParser st Integer
number = read `fmap` many1 digit <?> "number"

