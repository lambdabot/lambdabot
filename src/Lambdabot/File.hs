module Lambdabot.File
    ( findLBFile
    , findOrCreateLBFile
    , outputDir
    ) where

import Lambdabot.Config.Core
import Lambdabot.Monad
import Lambdabot.Util

import Control.Applicative
import Control.Monad
import Paths_lambdabot
import System.Directory
import System.FilePath

-- | Constants.
lambdabot :: FilePath
lambdabot = ".lambdabot"

stateDir :: LB FilePath
stateDir = (lambdabot </>) <$> getConfig outputDir

maybeFileExists :: FilePath -> IO (Maybe FilePath)
maybeFileExists path = do
    b <- doesFileExist path
    return $! if b then Just path else Nothing

-- | For a given file, look locally under State/. That is, suppose one is
-- running out of a Lambdabot darcs repository in /home/cale/lambdabot. Then
--
-- > lookLocally "fact" ~> "/home/cale/lambdabot/State/fact"
lookLocally :: FilePath -> LB (Maybe String)
lookLocally file = do
    local <- getConfig outputDir
    io $ maybeFileExists (local </> file)

-- | For a given file, look at the home directory. By default, we stash files in
-- ~/.lambdabot. So, running Lambdabot normally would let us do:
--
-- > lookHome "fact" ~> "/home/cale/lambdabot/State/fact"
--
-- (Note that for convenience we preserve the "State/foo" address pattern.)
lookHome :: FilePath -> LB (Maybe String)
lookHome f = do
    home    <- io getHomeDirectory
    state   <- stateDir
    io (maybeFileExists $ home </> state </> f)

-- | Do ~/.lambdabot & ~/.lambdabot/State exist?
isHome :: LB Bool
isHome = do
    home  <- io getHomeDirectory
    state <- stateDir
    io . fmap and . mapM (doesDirectoryExist . (home </>)) $ [lambdabot, state]

-- | Create ~/.lambdabot and ~/.lambdabot/State
mkdirL :: LB ()
mkdirL = do
    home  <- io getHomeDirectory
    state <- stateDir
    
    io . mapM_ (createDirectory . (home </>)) $ [lambdabot, state]

-- | Ask Cabal for the read-only copy of a file, and copy it into ~/.lambdabot/State.
-- if there isn't a read-only copy, create an empty file.
cpDataToHome :: FilePath -> LB ()
cpDataToHome f = do 
    local   <- getConfig outputDir
    state   <- stateDir
    
    rofile  <- io (getDataFileName (local </> f))
    home    <- io getHomeDirectory
    -- cp /.../lambdabot-4.foo/State/foo ~/.lambdabot/State/foo
    
    let outFile = home </> state </> f
    exists  <- io (doesFileExist rofile)
    if exists
        then io (copyFile rofile outFile)
        else io (writeFile outFile "")

-- | Try to find a pre-existing file, searching first in ./State and then in 
-- ~/.lambdabot/State
findLBFile :: FilePath -> LB (Maybe String)
findLBFile f = do
    first <- lookLocally f
    case first of
        -- With any luck we can exit quickly
        Just a -> return (Just a)
        -- OK, we didn't get lucky with local, so
        -- hopefully it's in ~/.lambdabot
        Nothing -> lookHome f

-- | Complicated. If a file exists locally, we return that. If a file exists in
-- ~/lambdabot/State, we return that. If neither the file nor ~/lambdabot/State
-- exist, we create the directories and then copy the file into it if a template
-- exists, or create an empty file if it does not.
-- Note that the return type is simple so we can just do a binding and stuff it
-- into the conventional functions easily; unfortunately, this removes
-- error-checking, as an error is now just \"\".
findOrCreateLBFile :: FilePath -> LB String
findOrCreateLBFile f = do
    mbFile <- findLBFile f
    case mbFile of
        Just file   -> return file
        -- Uh oh. We didn't find it locally, nor did we
        -- find it in ~/.lambdabot/State. So now we
        -- need to make ~/.lambdabot/State and copy it in.
        Nothing -> do
            exists <- isHome
            when (not exists) mkdirL
            cpDataToHome f
            -- With the file copied/created,
            -- a second attempt should work.
            g <- lookHome f
            case g of
                Just a -> return a
                Nothing -> do
                    home  <- io getHomeDirectory 
                    state <- stateDir
                    fail $ "findOrCreateLBFile: couldn't find file " 
                        ++ f ++ " in " ++ home </> state
