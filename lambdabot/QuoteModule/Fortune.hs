{-# OPTIONS -cpp #-}

module QuoteModule.Fortune where

import Util (stdGetRandItem)
import qualified Util hiding (getRandItem)

import Data.List
import Control.Monad
import System.Directory
import qualified Control.Exception as C (catch)

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

-- | Select a random fortune file
fileRandom :: IO FilePath
fileRandom = filelist >>= stdGetRandItem

-- | Parse a file of fortunes into a list of the fortunes in the file.
fortunesParse :: FilePath -> IO [String]
fortunesParse filename = do
    rawfs <- C.catch (readFile filename)
                     (\_ -> return "Couldn't find fortune file")
    return $ Util.split "%\n" rawfs

-- | Given a FilePath of a fortune file, select and return a random fortune from
--   it.
fortuneRandom :: FilePath -> IO String
fortuneRandom filename
    = do
      fortunesList <- fortunesParse filename
      stdGetRandItem fortunesList

-- | Given an optional fortune section, return a random fortune. If Nothing,
--   then a random fortune from all fortune files is returned. If Just section,
--   then a random fortune from the given section is returned.
randFortune :: (Maybe FilePath) -> IO String
randFortune section =
  case section of Nothing -> fortuneRandom =<< fileRandom
	          Just fname -> fortuneRandom =<< (return (path ++ fname))

-- | 'isFile' is a predicate wheter or not a given FilePath is a file.
isFile :: FilePath -> IO Bool
isFile x = do
           fs <- getFileStatus x
           return $ isRegularFile fs
