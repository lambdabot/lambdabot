{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

-- Copyright (c) 2005 Stefan Wehr (http://www.stefanwehr.de)
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Watch darcs patches arriving...
module Plugin.DarcsPatchWatch (theModule) where

import Plugin

import qualified Data.ByteString.Char8 as P
import Prelude hiding ( catch )

import Control.Concurrent (forkIO, killThread, modifyMVar_, readMVar, threadDelay, MVar, ThreadId)
import Control.OldException
import Control.Monad       ( when )

import System.Directory
import System.Time
import Message

$(plugin "DarcsPatchWatch")


--
-- Configuration variables
--

debugFlag :: Bool
debugFlag = False

maxNumberOfRepos :: Int
maxNumberOfRepos = 20

announceTarget :: Nick
announceTarget = Nick {
  nTag  = "freenode",
  nName = "#haskell"
  }

inventoryFile :: String
inventoryFile = "_darcs/inventory"

-- in seconds
checkInterval :: Int
checkInterval = 30

darcsCmd :: String
darcsCmd = "darcs"

--
-- The repository data type
--

-- Repositories is a list of Repo's
type Repos = [Repo]

-- A repository has a location, a time where it was last (known to be)
-- announced and a number of lines for the last announcement.
data Repo = Repo { repo_location     :: FilePath
                 , repo_lastAnnounced :: Maybe CalendarTime
                 , repo_nlinesAtLastAnnouncement :: Int }
            deriving (Eq,Ord,Show,Read)

-- | 'showRepo' takes a repository, repo, to a String for pretty printing.
showRepo :: Repo -> String
showRepo repo =
    "{Repository " ++ show (repo_location repo) ++ ", last announcement: " ++
    (case repo_lastAnnounced repo of
       Nothing -> "unknown"
       Just ct -> formatTime ct) ++ "}"

-- | 'showRepos' is the logical lifting of showRepo to to list types.
showRepos :: Repos -> String
showRepos [] = "{no repositories defined}"
showRepos l = '{' : ((concat . intersperse ", " . map showRepo) l ++ "}")

--
-- The state of the plugin
--

-- A darcs repository watcher state.
data DarcsPatchWatchState = DarcsPatchWatchState
                          { dpw_threadId  :: Maybe ThreadId
                          , dpw_repos     :: Repos }

-- DPW is our monadic state transformer.
type DPW a = ModuleT DarcsPatchWatchState LB a

-- Serialize and unserialize DarcsPatchWatch state.
stateSerial :: Serial DarcsPatchWatchState
stateSerial = Serial ser deSer
    where ser (DarcsPatchWatchState _ repos) = Just . P.pack $ show repos
          deSer s =
              do repos <- readM (P.unpack s)
                 return $ (DarcsPatchWatchState
                           { dpw_threadId = Nothing
                           , dpw_repos = repos })

getRepos :: DPW Repos
getRepos = dpw_repos `fmap` readMS

-- | 'withRepos' operates on the current state of the repos with a
--   given function.
withRepos :: (Repos -> (Repos -> LB ()) -> LB a) -- ^ Function to apply
          -> DPW a
-- template haskell?
withRepos = accessorMS $ \s -> (dpw_repos s, \t -> s { dpw_repos = t })



--
-- The plugin itself
--

instance Module DarcsPatchWatchModule DarcsPatchWatchState where

    moduleCmds  _ = ["repos", "repo-add", "repo-del"]

    moduleHelp    _ s = case s of
        "repos"        -> "repos. List all registered darcs repositories"
        "repo-add"     -> "repo-add <path>. Add a repository"
        "repo-del"     -> "repo-del <path>. Delete a repository"
        _              -> "Watch darcs repositories. Provides: repos, repo-add, repo-del"

    moduleSerialize _ = Just stateSerial
    moduleDefState  _ = return (DarcsPatchWatchState Nothing [])
    moduleInit      _ = do
      tid <- watchRepos -- must return thread ID.
      modifyMS (\s -> s { dpw_threadId = Just tid })

    moduleExit      _ =
        do s <- readMS
           case dpw_threadId s of
             Nothing  -> return ()
             Just tid -> io (killThread tid)

    process_ _ cmd rest = case cmd of
                         "repos"       -> printRepos rest
                         "repo-add"    -> addRepo rest
                         "repo-del"    -> delRepo rest


--
-- Configuration commands
--
printRepos :: String -> DPW [String]
printRepos [] = getRepos >>= return . (:[]) . showRepos
printRepos _  = error "@todo given arguments, try @todo-add or @list todo"

addRepo :: String -> DPW [String]
addRepo rest | null (dropSpace rest) = return ["argument required"]
addRepo rest = do
   x <- lift $ mkRepo rest
   case x of
     Right r -> withRepos $ \repos setRepos -> case () of {_
            | length repos >= maxNumberOfRepos ->
                return ["maximum number of repositories reached!"]
            | r `elem` repos ->
                return ["cannot add already existing repository " ++ showRepo r]
            | otherwise ->
                do setRepos (r:repos)
                   return ["repository " ++ showRepo r ++ " added"]
            }

     Left s  -> return ["cannot add invalid repository: " ++ s]

delRepo :: String -> DPW [String]
delRepo rest | null (dropSpace rest) = return ["argument required"]
delRepo rest = do
   x <- lift $ mkRepo rest
   case x of
     Left s -> return ["cannot delete invalid repository: " ++ s]
     Right r -> withRepos $ \repos setRepos -> case findRepo r repos of
                 Nothing ->
                   return ["no repository registered with path " ++ repo_location r]
                 Just realRepo ->
                     do setRepos (delete realRepo repos)
                        return ["repository " ++ showRepo realRepo ++ " deleted"]

    where
      cmpRepos r1 r2    = repo_location r1 == repo_location r2
      findRepo _ []     = Nothing
      findRepo x (y:ys) = if cmpRepos x y then Just y else findRepo x ys

mkRepo :: String -> LB (Either String Repo)
mkRepo pref_ =
    do x <- io $ do let pth  = mkInventoryPath pref_
                        pref = dropSpace pref_
                    perms <- getPermissions pth
                    return (Right (pref, perms))
                      `catch` (\e -> return $ Left (show e))
       case x of
         Left e -> return $ Left e
         Right (pref, perms)
             | readable perms -> return $ Right $ Repo pref Nothing 0
             | otherwise ->
                 return $ Left ("repository's inventory file not readable")


--
-- The heart of the plugin: watching darcs repositories
--

watchRepos :: DPW ThreadId
watchRepos = do
      ref <- getRef
      lift . forkForeverLB $ helper ref
    where sleepTime :: Int  -- in milliseconds
          sleepTime = checkInterval * 1000 * 1000
          helper    :: MVar DarcsPatchWatchState -> LB () -- !
          helper ref = do
              repos <- io . liftM dpw_repos . readMVar $ ref
              debug ("checking darcs repositories " ++ showRepos repos)
              repos_ <- mapM checkRepo repos
              io $ modifyMVar_ ref (\s -> return s{dpw_repos = repos_})
              io $ threadDelay sleepTime
              helper ref
          -- | run an IO action in another thread, with a timeout, lifted into LB
          forkForeverLB :: LB a -> LB ThreadId
          forkForeverLB f = (`liftLB` f) $ \g -> do
                      forkIO $ do
                          g
                          return ()


-- actually work out if we need to send a message
--
checkRepo :: Repo -> LB Repo
checkRepo r = do
       (output, errput) <- io $ runDarcs (repo_location r)
       nlines <-
         if not (null errput)
            then do info ("\ndarcs failed: " ++ errput)
                    return (repo_nlinesAtLastAnnouncement r)
            else do let olines = lines output
                        lastN = repo_nlinesAtLastAnnouncement r
                        new = take (length olines - lastN) olines
                    when (not (null new)) $
                        send' $ mkMsg (repo_location r) (parseDarcsMsg (unlines new))
                    return (length olines)

       now <- io getClockTime
       ct <- io $ toCalendarTime now
       return $ r { repo_nlinesAtLastAnnouncement = nlines
                  , repo_lastAnnounced = Just ct }
    where
       send' = ircPrivmsg announceTarget

mkMsg :: String -> (String,String,Integer) -> String
mkMsg r (who,msg,0) = "[" ++ basename r ++ ":" ++ who ++ "] " ++ msg
mkMsg r (who,msg,n) = (mkMsg r (who,msg,0)) ++ " (and "++show n++" more)"

runDarcs :: FilePath -> IO (String, String)
runDarcs loc = do
        (output, errput, _) <- popen darcsCmd ["changes", "--repo=" ++ loc] Nothing
        when (not (null errput)) $ info errput
        return (output, errput)

-- Extract the committer, and commit msg from the darcs msg
parseDarcsMsg :: String -> (String,String,Integer)
parseDarcsMsg s =
    let (_,rest)   = breakOnGlue "  " s -- two spaces
        (who,msg') = breakOnGlue "\n" rest
        (msg,n)    = countRest (drop 1 msg')
        who'       = if '@' `elem` who then fst (breakOnGlue "@" who) else who
    in (dropSpace who', drop 2 (dropSpace msg),n)
    where
        countRest  t = let (m,r) = breakOnGlue "\n" t in (m, countRest' r)

        countRest' []               = 0
        countRest' (' ':'*':' ':cs) = 1 + countRest' cs
        countRest' (_:cs)           = countRest' cs

--
-- Helpers
--

mkInventoryPath :: String -> FilePath
mkInventoryPath prefix =
    let pref = dropSpace prefix
        in joinPath pref inventoryFile

debug :: MonadIO m => String -> m ()
debug s = if debugFlag
             then io (putStrLn ("[DarcsPatchWatch] " ++ s))
             else return ()

info :: MonadIO m => String -> m ()
info s = io $ putStrLn ("[DarcsPatchWatch] " ++ s)

formatTime :: CalendarTime -> String
formatTime = calendarTimeToString

