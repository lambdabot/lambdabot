{-# OPTIONS -cpp #-}

module QuoteModule.Fortune where

import Data.List
#if __GLASGOW_HASKELL__ >= 600
import System.Posix
#else
import Posix
#endif
import Monad
import System.Directory
import System.Random
-- 	$Id: Fortune.hs,v 1.3 2003/07/29 13:41:49 eleganesh Exp $	

filelist :: IO [String]
filelist = do filelist' <- getDirectoryContents "/usr/share/games/fortunes"
              let files = filter (not . isSuffixOf ".dat") filelist'
              join (return (filterM isFile (map (path ++) files)))

choose :: (Fractional b, Integral a1, Integral a) => a1 -> a -> b
choose listLength randomInt
        = ((fromIntegral randomInt) + 2147483647) * (fromIntegral listLength) / 4294967295

getRandItem :: (RandomGen g) => [a] -> g -> (a, g)
getRandItem mylist rng = ((mylist !! floor ((choose llen rndInt) :: Double)),newRng)
                         where
                         llen = length mylist
                         rndInt = fst (next rng)
                         newRng = snd (next rng)

fileRandom :: (RandomGen a) => a -> IO (String, a)
fileRandom rng
    = do
      randPair <- liftM2 getRandItem filelist (return rng)
      return randPair

path :: [Char]
path = "/usr/share/games/fortunes/"

fortunesParse :: FilePath -> IO [String]
fortunesParse filename = do rawfs <- readFile filename
                            return (map unlines $ splines $ lines rawfs)

fortuneRandom :: (RandomGen g) => FilePath -> g -> IO (String, g)
fortuneRandom filename rng
    = do
      fortunesList <- fortunesParse filename
      return (getRandItem fortunesList rng)

randFortune :: (RandomGen a) => a -> IO (String, a)
randFortune rng = join (liftM (uncurry fortuneRandom) $ fileRandom rng)

plines     :: String -> [String]
plines ""   = []
plines s    = let (l,s') = break ('\n'==) s
             in l : case s' of []      -> []
                               (_:s'') -> lines s''
splines :: [[Char]] -> [[[Char]]]
splines [[]] = []
splines s    =  let 
                (l,s') = break ("%"==) s
                in l : case s' of []      -> []
                                  (_:s'') -> splines s''


-- isFile x = do
--            liftM2 (==) (fileType x) (return RegularFile)

isFile :: FilePath -> IO Bool
isFile x = do
           fs <- getFileStatus x
           return $ isRegularFile fs
