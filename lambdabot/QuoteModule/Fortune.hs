{-# OPTIONS -cpp #-}

module QuoteModule.Fortune where

import Data.List
import Monad
import System.Directory
import System.Random
import qualified Control.Exception as C (catch)
import QuoteModule.Random
import qualified Util

--
-- No good for win32
--
#if __GLASGOW_HASKELL__ >= 600
import System.Posix
#else
import Posix
#endif

-- 	$Id: Fortune.hs,v 1.3 2003/07/29 13:41:49 eleganesh Exp $

-- | The 'path' component is a string to the location where the fortune files
--   are located. On some systems, this is /usr/share/games/fortunes, on others
--   this is /usr/share/games/fortune. Alter this to suit your configuration
--
--   TODO: Move this to a generic Config file
path :: String
path = "/usr/share/games/fortunes/"

-- | The 'filelist' function returns a List of fortune files from the
--   configured 'path' directory.
filelist :: IO [String]
filelist = do filelist'<- C.catch (getDirectoryContents path)
                                  (\_ -> return [])
              let files = filter (not . isSuffixOf ".dat") filelist'
              join (return (filterM isFile (map (path ++) files)))

-- | Select a random fortune file by using the random number generator
--   given.
fileRandom :: (RandomGen a) => a -> IO (String, a)
fileRandom rng = liftM2 getRandItem filelist (return rng)

-- | Parse a file of fortunes into a list of the fortunes in the file.
fortunesParse :: FilePath -> IO [String]
fortunesParse filename = do
    rawfs <- C.catch (readFile filename)
                     (\_ -> return "Couldn't find fortune file")
    return $ Util.split "%\n" rawfs

-- | Given a FilePath of a fortune file, select a random fortune from it
--   and return it along with the altered new seed of the RNG.
fortuneRandom :: (RandomGen g) => FilePath -> g -> IO (String, g)
fortuneRandom filename rng
    = do
      fortunesList <- fortunesParse filename
      return (getRandItem fortunesList rng)

-- | Given a RNG and optionally a fortune section, return a random fortune
--   from the all the fortune files or the given section, respectively.
randFortune :: (RandomGen a) => (Maybe FilePath) -> a -> IO (String, a)
randFortune section rng =
    let randomF f = join (liftM (uncurry fortuneRandom) $ f)
    in case section of Nothing -> randomF (fileRandom rng)
	               Just fname -> randomF (return $ (path ++ fname, rng))

-- | Split a fortune file into a list of fortures by breaking it up on
--   the '%' characters
splines :: [[Char]] -> [[[Char]]]
splines [[]] = []
splines s    =  let (l,s') = break ("%"==) s
                in l : case s' of []      -> []
                                  (_:s'') -> splines s''

-- | 'isFile' is a predicate wheter or not a given FilePath is a file.
isFile :: FilePath -> IO Bool
isFile x = do
           fs <- getFileStatus x
           return $ isRegularFile fs
