module File where

import Control.Monad
import System.Directory
import System.FilePath
import Paths_lambdabot (getDataFileName)

-- | Constants.
lambdabot, local, state :: FilePath
lambdabot = ".lambdabot"
state = lambdabot </> local
local = "State"

maybeFileExists :: FilePath -> IO (Maybe FilePath)
maybeFileExists path = 
    do b <- doesFileExist path
       return $ if b then Just path else Nothing

-- | For a given file, look locally under State/. That is, suppose one is
-- running out of a Lambdabot darcs repository in /home/cale/lambdabot. Then
--
-- > lookLocally "fact" ~> "/home/cale/lambdabot/State/fact"
lookLocally :: FilePath -> IO (Maybe String)
lookLocally file = maybeFileExists $ local </> file

-- | For a given file, look at the home directory. By default, we stash files in
-- ~/.lambdabot. So, running Lambdabot normally would let us do:
--
-- > lookHome "fact" ~> "/home/cale/lambdabot/State/fact"
--
-- (Note that for convenience we preserve the "State/foo" address pattern.)
lookHome :: FilePath -> IO (Maybe String)
lookHome f = do home <- getHomeDirectory
                maybeFileExists $ home </> state </> f

-- | Do ~/.lambdabot & ~/.lambdabot/State exist?
isHome :: IO Bool
isHome = do home <- getHomeDirectory
            fmap and . mapM (doesDirectoryExist . (home </>)) $ [lambdabot, state]

-- | Create ~/.lambdabot and ~/.lambdabot/State
mkdirL :: IO ()
mkdirL = do home <- getHomeDirectory
            mapM_ (createDirectory . (home </>)) $ [lambdabot, state]

-- | Ask Cabal for the read-only copy of a file, and copy it into ~/.lambdabot/State.
cpDataToHome :: FilePath -> IO ()
cpDataToHome f = do rofile <- getDataFileName (local </> f)
                    home <- getHomeDirectory
                    -- cp /.../lambdabot-4.foo/State/foo ~/.lambdabot/State/foo
                    copyFile rofile (home </> state </> f)

-- | Complicated. If a file exists locally, we return that. If a file exists in
-- ~/lambdabot/State, we return that. If neither the file nor ~/lambdabot/State
-- exist, we create the directories and then copy the file into it.
-- Note that the return type is simple so we can just do a binding and stuff it
-- into the conventional functions easily; unfortunately, this removes
-- error-checking, as an error is now just \"\".
findFile :: FilePath -> IO String
findFile f = do first <- lookLocally f
                case first of
                  -- With any luck we can exit quickly
                  Just a -> return a
                  Nothing -> do second <- lookHome f
                                case second of
                                  -- OK, we didn't get lucky with local, so
                                  -- hopefully it's in ~/.lambdabot
                                  Just a -> return a
                                  -- Uh oh. We didn't find it locally, nor did we
                                  -- find it in ~/.lambdabot/State. So now we
                                  -- need to make ~/.lambdabot/State and copy it in.
                                  Nothing -> do exists <- isHome
                                                when (not exists) mkdirL
                                                cpDataToHome f
                                                -- With the file copied/created,
                                                -- a second attempt should work.
                                                g <- lookHome f
                                                case g of
                                                  Just a -> return a
                                                  Nothing -> 
                                                      do home <- getHomeDirectory 
                                                         fail $ "File.findFile: couldn't find file " 
                                                                ++ f ++ " in " ++ home </> state
