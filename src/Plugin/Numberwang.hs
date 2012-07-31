module Plugin.Numberwang where

import Control.Applicative
import Data.Random
import Data.Random.Distribution.Poisson
import Numeric
import Plugin


data NumberwangState = State
    { nextCmd   :: !Int -- number of invocations of @numberwang before the next numberwang
    , nextCon   :: !Int -- number of contextual occurrences of numbers before next numberwang
    }

cmdDist = poisson (3.5 :: Double)
conDist = poisson (32  :: Double)

theModule = newModule
    { moduleDefState = sample (State <$> cmdDist <*> conDist)
    , moduleCmds = return
        [ (command "numberwang")
            { help = say "@numberwang <number>: Determines if it is Numberwang."
            , process = doNumberwang True . length . words
            }
        ]
    , contextual = doNumberwang False . length . numbers
    }

numbers :: RealFrac t => String -> [t]
numbers [] = []
numbers cs = case readFloat cs of
    (n, rest):_ -> n : numbers rest
    _           -> numbers (tail cs)

doNumberwang cmd n
    | n <= 0    = when cmd $ say "What number?"
    | otherwise = do
        isNumberwang <- checkNumberwang cmd 1
        if isNumberwang
            then say "That's Numberwang!"
            else when cmd $ say "Sorry, that's not Numberwang."

withState True f = withMS $ \st setST ->
    f (nextCmd st) (\n -> setST st {nextCmd = n}) cmdDist
withState False f = withMS $ \st setST ->
    f (nextCon st) (\n -> setST st {nextCon = n}) conDist

checkNumberwang cmd l = withState cmd $ \ n setN nDist -> do
    if n <= l
        then do
            setN =<< sample nDist
            return True
        else do
            setN (n - l)
            return False
