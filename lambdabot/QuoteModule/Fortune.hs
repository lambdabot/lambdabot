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
filelist = do filelist <- getDirectoryContents "/usr/share/games/fortunes"
              let files = filter (not . isSuffixOf ".dat") filelist
              join (return (filterM isFile (map (path ++) files)))

choose listLength randomInt = ((fromIntegral randomInt) + 2147483647) * (fromIntegral listLength) / 4294967295


getRandItem mylist rng = ((mylist !! floor (choose llen rndInt)),newRng)
                         where
                         llen = length mylist
                         rndInt = fst (next rng)
                         newRng = snd (next rng)

fileRandom rng 
    = do
      randPair <- liftM2 getRandItem filelist (return rng)
      return randPair

path = "/usr/share/games/fortunes/"

fortunesParse filename = do rawfs <- readFile filename
                            return (map unlines $ splines $ lines rawfs)

fortuneRandom filename rng 
    = do
      fortunesList <- fortunesParse filename
      return (getRandItem fortunesList rng)

randFortune rng = do 
                  join (liftM (uncurry fortuneRandom) $ fileRandom rng)

plines     :: String -> [String]
plines ""   = []
plines s    = let (l,s') = break ('\n'==) s
             in l : case s' of []      -> []
                               (_:s'') -> lines s''
splines [[]] = []
splines s    =  let 
                (l,s') = break ("%"==) s
                in l : case s' of []      -> []
                                  (_:s'') -> splines s''


-- isFile x = do
--            liftM2 (==) (fileType x) (return RegularFile)

isFile x = do
           fs <- getFileStatus x
           return $ isRegularFile fs
