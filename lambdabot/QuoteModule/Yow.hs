module QuoteModule.Yow where

import Monad
import System.Random
import Util
import qualified Control.Exception as C (catch)
import QuoteModule.Random

-- 	$Id: Yow.hs,v 1.3 2003/07/29 13:41:50 eleganesh Exp $

path :: [Char]
path = "yow.lines"

yowParse :: FilePath -> IO [[Char]]
yowParse filename = do
        rawfs <- C.catch (readFile filename)
                         (\_ -> return "Couldn't find yow file")
        return ( Util.split "\00" rawfs)

yowRandom :: (RandomGen g) => g -> IO ([Char], g)
yowRandom rng
    = do
      yowList <- yowParse path
      return (getRandItem yowList rng)
