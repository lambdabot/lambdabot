{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Plugin.Numberwang where

import Data.Random
import Data.Random.Distribution.Poisson
import Numeric
import Plugin

plugin "Numberwang"

instance Module NumberwangModule where
    type ModuleState NumberwangModule = Int
    moduleDefState _ = resetState
    
    moduleCmds = return
        [ (command "numberwang")
            { help = say "@numberwang <number>: Determines if it is Numberwang."
            , process = doNumberwang True . length . words
            }
        ]
    contextual _ = doNumberwang False . length . numbers

numbers :: RealFrac t => String -> [t]
numbers [] = []
numbers cs = case readFloat cs of
    (n, rest):_ -> n : numbers rest
    _           -> numbers (tail cs)

doNumberwang loudly n
    | n <= 0    = when loudly $ say "What number?"
    | otherwise = do
        isNumberwang <- checkNumberwang n
        if isNumberwang
            then say "That's Numberwang!"
            else when loudly $ say "Sorry, that's not Numberwang."

numberwangRate = 4.5 :: Double
resetState = sample (poisson numberwangRate)

checkNumberwang l = withMS $ \ n setN -> do
    if n <= l
        then do
            setN =<< resetState
            return True
        else do
            setN (n - l)
            return False
