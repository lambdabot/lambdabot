--
-- Copyright (c) 2005 Stefan Wehr (http://www.stefanwehr.de)
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Watch darcs patches arriving...
--
module Plugins.DarcsPatchWatch (theModule) where

import Prelude hiding ( catch )
import List ( intersperse, delete )
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans ( liftIO, MonadIO )
import System.Directory
import System.Time

import Lambdabot hiding (send)
import LBState
import Util
import PosixCompat ( popen )

newtype DarcsPatchWatch = DarcsPatchWatch ()

theModule :: MODULE
theModule = MODULE $ DarcsPatchWatch ()


--
-- Configuration variables
--

debugFlag :: Bool
debugFlag = False

announceTarget :: String
announceTarget = "#00maja"

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

type Repos = [Repo]

data Repo = Repo { repo_location     :: FilePath
                 , repo_lastAnnounced :: Maybe CalendarTime
                 , repo_nlinesAtLastAnnouncement :: Int }
            deriving (Eq,Ord,Show,Read)

showRepo :: Repo -> String
showRepo repo = 
    "{Repository " ++ show (repo_location repo) ++ ", last announcement: " ++
    (case repo_lastAnnounced repo of
       Nothing -> "unknown"
       Just ct -> formatTime ct) ++ "}"

showRepos :: Repos -> String
showRepos [] = "{no repositories defined}"
showRepos l = '{' : ((concat . intersperse ", " . map showRepo) l ++ "}")


--
-- The state of the plugin
--

data DarcsPatchWatchState = DarcsPatchWatchState
                          { dpw_threadId  :: Maybe ThreadId
                          , dpw_repos     :: Repos }

stateSerializer :: Serializer DarcsPatchWatchState
stateSerializer = 
    Serializer { serialize = Just . ser
               , deSerialize = deSer }
    where ser (DarcsPatchWatchState _ repos) = show repos
          deSer s = 
              do repos <- readM s
                 return $ (DarcsPatchWatchState 
                           { dpw_threadId = Nothing
                           , dpw_repos = repos })

getRepos :: DWP Repos
getRepos = 
    do s <- readMS
       return (dpw_repos s)

setRepos :: (?name::String, ?ref::MVar DarcsPatchWatchState) => Repos -> LB ()
setRepos repos = 
    do modifyMS (\s -> s { dpw_repos = repos })

type DWP a = ModuleT DarcsPatchWatchState LB a

--
-- The plugin itself
--

instance Module DarcsPatchWatch DarcsPatchWatchState where
    moduleHelp    _ s = return $ case s of
        "repos"        -> "@repos, list all registered darcs repositories"
        "repo-add"    -> "@repo-add path, add a repository"
        "repo-del" -> "@repo-del path, delete a repository" 
        _ -> ("Watch darcs repositories. Provides @repos, @repo-add, @repo-del")

    moduleCmds  _ = return ["repos", "repo-add", "repo-del"] 

    moduleDefState  _ = return (DarcsPatchWatchState Nothing [])
    moduleSerialize _ = Just stateSerializer

    moduleInit      _ = do
      tid <- lbIO (\conv -> forkIO $ conv watchRepos)
      modifyMS (\s -> s { dpw_threadId = Just tid })
    moduleExit      _ = 
        do s <- readMS
           case dpw_threadId s of
             Nothing -> return ()
             Just tid ->
                 liftIO $ killThread tid

    process _ _ source cmd rest =
           case cmd of
             "repos"       -> printRepos source rest
             "repo-add"    -> addRepo source rest
             "repo-del"    -> delRepo source rest
             _ -> error "unimplemented command"

--
-- Configuration commands
--

printRepos :: String -> String -> DWP ()
printRepos source "" = 
    do repos <- getRepos
       ircPrivmsg source (showRepos repos)
printRepos _ _ =
    error "@todo given arguments, try @todo-add or @listcommands todo"

addRepo :: String -> String -> DWP ()
addRepo source rest | null (dropSpace rest) = 
    ircPrivmsg source "argument required"
addRepo source rest = 
    do x <- mkRepo rest
       case x of
         Left s -> send ("cannot add invalid repository: " ++ s)
         Right r -> do repos <- getRepos
                       if r `elem` repos
                          then send ("cannot add already existing repository " 
                                     ++ showRepo r)
                          else
                          do setRepos (r:repos)
                             send ("repository " ++ showRepo r ++ " added")
    where send = ircPrivmsg source

delRepo :: String -> String -> DWP ()
delRepo source rest | null (dropSpace rest) = 
    ircPrivmsg source "argument required"
delRepo source rest = 
    do x <- mkRepo rest
       case x of
         Left s -> ircPrivmsg source ("cannot delete invalid repository: " ++ s)
         Right r -> do repos <- getRepos
                       case findRepo r repos of
                         Nothing ->
                           send ("no repository registered with path " 
                                 ++ repo_location r)
                         Just realRepo ->
                             do setRepos (delete realRepo repos)
                                send ("repository " ++ showRepo realRepo ++ 
                                      " deleted")
    where send = ircPrivmsg source
          cmpRepos r1 r2 = repo_location r1 == repo_location r2
          findRepo _ [] = Nothing
          findRepo x (y:ys) = if cmpRepos x y then (Just y) else findRepo x ys

mkRepo :: String -> DWP (Either String Repo)
mkRepo pref' =
    do x <- liftIO $ do let path = mkInventoryPath pref'
                            pref = dropSpace pref'
                        perms <- getPermissions path
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

watchRepos :: DWP ()
watchRepos = 
    do repos <- getRepos
       debug ("checking darcs repositories " ++ showRepos repos)
       repos' <- mapM checkRepo repos
       setRepos repos'
       liftIO $ threadDelay sleepTime
       watchRepos
    where sleepTime :: Int  -- in milliseconds
          sleepTime = checkInterval * 1000 * 1000



checkRepo :: Repo -> DWP Repo
checkRepo repo = 
    do mtime <- liftIO $ getModificationTime (repo_location repo)
       case repo_lastAnnounced repo of
         Nothing                           -> announceRepoChanges repo
         Just ct | toClockTime ct <= mtime -> announceRepoChanges repo
                 | otherwise               -> return repo

announceRepoChanges :: Repo -> DWP Repo
announceRepoChanges r = 
    do let header = "Changes have been made to " ++ repo_location r
       now <- liftIO getClockTime
       (output, errput) <- liftIO $ runDarcs (repo_location r)
       nlines <- 
           if not (null errput)
              then do send (header ++ "\ndarcs failed: " ++ errput)
                      return (repo_nlinesAtLastAnnouncement r)
              else let olines = lines output
                       lastN = repo_nlinesAtLastAnnouncement r
                       new = take (length olines - lastN) olines
                   in do if null new
                            then info ("silently ignoring that darcs hasn't " ++
                                        "produced any new lines since last check")
                            else send (header ++ "\n" ++ unlines new)
                         return (length olines)
       ct <- liftIO $ toCalendarTime now
       return $ r { repo_nlinesAtLastAnnouncement = nlines
                  , repo_lastAnnounced = Just ct }
    where send s = ircPrivmsg announceTarget s

runDarcs :: FilePath -> IO (String, String)
runDarcs loc =
    do (output, errput, _) <- popen darcsCmd ["changes", "--repo=" ++ loc]
                                Nothing
       if not (null errput)
          then info errput
          else return ()
       return (output, errput)


--
-- Helpers
--

mkInventoryPath :: String -> FilePath
mkInventoryPath prefix = 
    let pref = dropSpace prefix
        in joinPath pref inventoryFile

joinPath :: FilePath -> FilePath -> FilePath
joinPath p q =
    case reverse p of
      '/':_ -> p ++ q
      []    -> q
      _     -> p ++ "/" ++ q

debug :: MonadIO m => String -> m ()
debug s = if debugFlag 
             then liftIO (putStrLn ("[DarcsPatchWatch] " ++ s)) 
             else return ()

info :: MonadIO m => String -> m ()
info s = liftIO $ putStrLn ("[DarcsPatchWatch] " ++ s)

formatTime :: CalendarTime -> String
formatTime = calendarTimeToString
