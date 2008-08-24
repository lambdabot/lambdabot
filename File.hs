module File where

import Control.Monad
import System.Directory
import Paths_lambdabot (getDataFileName)

-- | Constants.
lambdabot, state :: String
lambdabot = "/.lambdabot"
state = "/.lambdabot/State"

-- | For a given file, look locally under State/. That is, suppose one is
-- running out of a Lambdabot darcs repository in /home/cale/lambdabot. Then
--
-- > lookLocally "fact" ~> "/home/cale/lambdabot/State/fact"
lookLocally :: FilePath -> IO (Maybe String)
lookLocally f = do b <- doesFileExist local
                   if b then return $ Just local else return Nothing
                    where local = "State/" ++ f

-- | For a given file, look at the home directory. By default, we stash files in
-- ~/.lambdabot. So, running Lambdabot normally would let us do:
--
-- > lookHome "fact" ~> "/home/cale/lambdabot/State/fact"
--
-- (Note that for convenience we preserve the "State/foo" address pattern.)
lookHome :: FilePath -> IO (Maybe String)
lookHome f = do home <- getHomeDirectory
                b <- doesFileExist (home ++ state ++ f)
                if b then return $ Just (state ++ f) else return Nothing

-- | Do ~/.lambdabot & ~/.lambdabot/State exist?
isHome :: IO Bool
isHome = do home <- getHomeDirectory
            top <- doesDirectoryExist (home ++ lambdabot)
            stat <- doesDirectoryExist (home ++ state)
            return (top && stat)

-- | Create ~/.lambdabot and ~/.lambdabot/State
mkdirL :: IO ()
mkdirL = do home <- getHomeDirectory
            createDirectory (home ++ lambdabot)
            createDirectory (home ++ state)

-- | Ask Cabal for the read-only copy of a file, and copy it into ~/.lambdabot/State.
cpDataToHome :: FilePath -> IO ()
cpDataToHome f = do rofile <- getDataFileName f
                    home <- getHomeDirectory
                    copyFile rofile (home ++ state)

-- | Complicated. If a file exists locally, we return that. If a file exists in
-- ~/lambdabot/State, we return that. If neither the file nor ~/lambdabot/State
-- exist, we create the directories and then copy the file into it.
findFile :: FilePath -> IO (Maybe String)
findFile f = do first <- lookLocally f
                case first of
                  Just a -> return $ Just a
                  Nothing -> do second <- lookHome f
                                case second of
                                  Just a -> return $ Just a
                                  -- Uh oh. We didn't find it locally, nor did we
                                  -- find it in ~/.lambdabot/State. So now we
                                  -- need to make ~/.lambdabot/State and copy it in.
                                  Nothing -> do exists <- isHome
                                                when (not exists) mkdirL
                                                cpDataToHome f
                                                lookHome f
                                return ()
                                lookHome f
                -- Finally, we pretend all the foregoing never happened, and
                -- ~/.lambdabot existed all along.
                lookHome f
