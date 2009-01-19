{-# LANGUAGE CPP #-}

-- | Fortune.hs, quote the fortune file
module Plugin.Quote.Fortune where

import Config
import Lambdabot.Util (stdGetRandItem, split)
import qualified Lambdabot.Util hiding (stdGetRandItem)

import Data.List
import Control.Monad
import System.Directory
import qualified Control.OldException as C (catch)

#ifndef mingw32_HOST_OS
--
-- No good for win32
--
import System.Posix (isRegularFile, getFileStatus)
#endif

-- | The 'filelist' function returns a List of fortune files from the
--   configured 'fortunePath' directory.
filelist :: IO [String]
filelist = do
    filelist'<- C.catch (getDirectoryContents $ fortunePath config)
                        (\_ -> return [])
    let files = filter (not . isSuffixOf ".dat") filelist'
    join (return (filterM isFile (map (fortunePath config ++) files)))

-- | Select a random fortune file
fileRandom :: IO FilePath
fileRandom = filelist >>= stdGetRandItem

-- | Parse a file of fortunes into a list of the fortunes in the file.
fortunesParse :: FilePath -> IO [String]
fortunesParse filename = do
    rawfs <- C.catch (readFile filename)
                     (\_ -> return "Couldn't find fortune file")
    return $ split "%\n" rawfs

-- | Given a FilePath of a fortune file, select and return a random fortune from
--   it.
fortuneRandom :: FilePath -> IO String
fortuneRandom = (stdGetRandItem =<<) . fortunesParse

-- | Given an optional fortune section, return a random fortune. If Nothing,
--   then a random fortune from all fortune files is returned. If Just section,
--   then a random fortune from the given section is returned.
randFortune :: (Maybe FilePath) -> IO String
randFortune section = case section of
    Nothing    -> fortuneRandom =<< fileRandom
    Just fname -> fortuneRandom =<< (return (fortunePath config ++ fname))

-- | 'isFile' is a predicate wheter or not a given FilePath is a file.
#ifdef mingw32_HOST_OS
isFile :: FilePath -> IO Bool
isFile = doesFileExist
#else
isFile :: FilePath -> IO Bool
isFile = (isRegularFile `fmap`) . getFileStatus
#endif
