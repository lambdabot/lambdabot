{-# LANGUAGE TypeFamilies #-}

module Lambdabot.Plugin.Numberwang where

import Control.Applicative
import Control.Monad
import Data.Random
import Data.Random.Distribution.Poisson
import Lambdabot.Plugin
import Numeric


data NumberwangState = State
    { nextCmd   :: !Int -- number of invocations of @numberwang before the next numberwang
    , nextCon   :: !Int -- number of contextual occurrences of numbers before next numberwang
    }

cmdDist :: RVar Int
cmdDist = poisson (3.5 :: Double)

conDist :: RVar Int
conDist = poisson (32  :: Double)

theModule :: Module NumberwangState
theModule = newModule
    { moduleDefState = sample (State <$> cmdDist <*> conDist)
    , moduleCmds = return
        [ (command "numberwang")
            { help = say "@numberwang <number>: Determines if it is Numberwang."
            , process = doNumberwang True . length . words
            }
        ]
    , contextual = doNumberwang False . length . (numbers :: String -> [Double])
    }

numbers :: RealFrac t => String -> [t]
numbers [] = []
numbers cs = case readFloat cs of
    (n, rest):_ -> n : numbers rest
    _           -> numbers (tail cs)

doNumberwang :: (Num a, Ord a, MonadLBState m, LBState m ~ NumberwangState) =>
                Bool -> a -> Cmd m ()
doNumberwang cmd n
    | n <= 0    = when cmd $ say "What number?"
    | otherwise = do
        isNumberwang <- checkNumberwang cmd 1
        if isNumberwang
            then say "That's Numberwang!"
            else when cmd $ say "Sorry, that's not Numberwang."

withState :: (MonadLBState m, LBState m ~ NumberwangState) =>
             Bool -> (Int -> (Int -> m ()) -> RVar Int -> m a) -> m a
withState True f = withMS $ \st setST ->
    f (nextCmd st) (\n -> setST st {nextCmd = n}) cmdDist
withState False f = withMS $ \st setST ->
    f (nextCon st) (\n -> setST st {nextCon = n}) conDist

checkNumberwang :: (MonadLBState m, LBState m ~ NumberwangState) =>
                   Bool -> Int -> m Bool
checkNumberwang cmd l = withState cmd $ \ n setN nDist -> do
    if n <= l
        then do
            setN =<< lb (sample nDist)
            return True
        else do
            setN (n - l)
            return False
