module QuoteModule.Random (
    getRandItem,
    choose
) where

import System.Random

randomRange :: Integer
randomRange = sum $ map (abs . fromIntegral) [minBound :: Int, maxBound :: Int]

choose :: (Fractional b, Integral a1, Integral a) => a -> a1 -> b
choose listLength randomInt
        = (fromIntegral $ listLength) * (fromIntegral randomInt) / (fromIntegral randomRange)

getRandItem :: (RandomGen g) => [a] -> g -> (a, g)
getRandItem mylist rng = (mylist !! index,newRng)
                         where
                         llen = length mylist
                         (index, newRng) = randomR (0,llen - 1) rng
