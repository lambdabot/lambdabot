{-# OPTIONS -cpp #-}

module QuoteModule.Fortune where

import Data.List
import Monad
import System.Directory
import System.Random
import qualified Control.Exception as C (catch)
import QuoteModule.Random

--
-- No good for win32
--
#if __GLASGOW_HASKELL__ >= 600
import System.Posix
#else
import Posix
#endif

-- 	$Id: Fortune.hs,v 1.3 2003/07/29 13:41:49 eleganesh Exp $

path :: String
path = "/usr/share/games/fortunes/"

filelist :: IO [String]
filelist = do filelist'<- C.catch (getDirectoryContents path)
                                  (\_ -> return [])
              let files = filter (not . isSuffixOf ".dat") filelist'
              join (return (filterM isFile (map (path ++) files)))

fileRandom :: (RandomGen a) => a -> IO (String, a)
fileRandom rng = liftM2 getRandItem filelist (return rng)

fortunesParse :: FilePath -> IO [String]
fortunesParse filename = do
    rawfs <- C.catch (readFile filename)
                     (\_ -> return "Couldn't find fortune file")
    return (map unlines $ splines $ lines rawfs)

fortuneRandom :: (RandomGen g) => FilePath -> g -> IO (String, g)
fortuneRandom filename rng
    = do
      fortunesList <- fortunesParse filename
      return (getRandItem fortunesList rng)

randFortune :: (RandomGen a) => a -> IO (String, a)
randFortune rng = join (liftM (uncurry fortuneRandom) $ fileRandom rng)

splines :: [[Char]] -> [[[Char]]]
splines [[]] = []
splines s    =  let (l,s') = break ("%"==) s
                in l : case s' of []      -> []
                                  (_:s'') -> splines s''

isFile :: FilePath -> IO Bool
isFile x = do
           fs <- getFileStatus x
           return $ isRegularFile fs
