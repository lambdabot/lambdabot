-- | Manage lambdabot's state files. There are three relevant directories:
--
-- * local: @./State/@ (configurable, see `outputDir`)
-- * home:  @~/.lambdabot/State/@
-- * data:  relative to the data directory of the @lambdabot@ package.
--
-- Files are stored locally if the directory exists; otherwise, in the home
-- directory. When reading a state file, and the file exists in the data
-- directory but nowhere else, then it is picked up from the data directory.

module Lambdabot.File
    ( stateDir
    , findLBFileForReading
    , findLBFileForWriting
    , findOrCreateLBFile
    , findLBFile -- deprecated
    , outputDir
    ) where

import Lambdabot.Config.Core
import Lambdabot.Monad
import Lambdabot.Util

import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath

lambdabot :: FilePath
lambdabot = ".lambdabot"

-- | Locate state directory. Returns the local directory if it exists,
-- and the home directory otherwise.
stateDir :: LB FilePath
stateDir = do
    -- look locally
    output <- getConfig outputDir
    b <- io $ doesDirectoryExist output
    if b then return output else homeDir

homeDir :: LB FilePath
homeDir = do
    output <- getConfig outputDir
    home <- io getHomeDirectory
    return $ home </> lambdabot </> output

-- | Look for the file in the local, home, and data directories.
findLBFileForReading :: FilePath -> LB (Maybe FilePath)
findLBFileForReading f = do
    state <- stateDir
    home  <- homeDir
    output <- getConfig outputDir
    rodir <- getConfig dataDir
    findFirstFile [state </> f, home </> f, rodir </> output </> f]

-- | Return file name for writing state. The file will reside in the
-- state directory (`stateDir`), and `findLBFileForWriting` ensures that
-- the state directory exists.
findLBFileForWriting :: FilePath -> LB FilePath
findLBFileForWriting f = do
    state <- stateDir
    -- ensure that the directory exists
    io $ createDirectoryIfMissing True state
    success <- io $ doesDirectoryExist state
    when (not success) $ fail $ concat ["Unable to create directory ", state]
    return $ state </> f

findFirstFile :: [FilePath] -> LB (Maybe FilePath)
findFirstFile [] = return Nothing
findFirstFile (path:ps) = do
    b <- io $ doesFileExist path
    if b then return (Just path) else findFirstFile ps

{-# DEPRECATED findLBFile
 "Use `findLBFileForReading` or `findLBFileForWriting` instead" #-}
-- | Try to find a pre-existing file, searching first in the local or home
-- directory (but not in the data directory)
findLBFile :: FilePath -> LB (Maybe String)
findLBFile f = do
    state <- stateDir
    home  <- homeDir
    findFirstFile [state </> f, home </> f]

-- | This returns the same file name as `findLBFileForWriting`.
-- If the file does not exist, it is either copied from the data (or home)
-- directory, if a copy is found there; otherwise, an empty file is
-- created instead.
findOrCreateLBFile :: FilePath -> LB String
findOrCreateLBFile f = do
    outFile <- findLBFileForWriting f
    b <- io $ doesFileExist outFile
    when (not b) $ do
        -- the file does not exist; populate it from home or data directory
        b <- findLBFileForReading f
        case b of
            Nothing      -> io $ writeFile outFile ""
            Just roFile  -> io $ copyFile roFile outFile
    return outFile
