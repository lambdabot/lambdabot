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
import Control.Monad       ( when )
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

maxNumberOfRepos :: Int
maxNumberOfRepos = 20

debugFlag :: Bool
debugFlag = False

announceTarget :: String
announceTarget = "#z123lambdabot"

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
showRepo :: Repo -- ^ Repository to pretty-print
	 -> String -- ^ Resulting string.
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

printRepos :: String -> String -> DPW ()
printRepos source "" =
    do repos <- getRepos
       ircPrivmsg source (showRepos repos)
printRepos _ _ =
    error "@todo given arguments, try @todo-add or @listcommands todo"

addRepo :: String -> String -> DPW ()
addRepo source rest | null (dropSpace rest) =
    ircPrivmsg source "argument required"
addRepo source rest =
    do x <- mkRepo rest
       case x of
         Left s -> send ("cannot add invalid repository: " ++ s)
         Right r -> withRepos $ \repos setRepos -> do
                       case () of
                        _| length repos >= maxNumberOfRepos ->
                             send ("maximum number of repositories reached!")
                         | r `elem` repos ->
                             send ("cannot add already existing repository "
                                   ++ showRepo r)
                         | otherwise ->
                             do setRepos (r:repos)
                                send ("repository " ++ showRepo r ++ " added")
    where send = ircPrivmsg source

delRepo :: String -> String -> DPW ()
delRepo source rest | null (dropSpace rest) =
    ircPrivmsg source "argument required"
delRepo source rest =
    do x <- mkRepo rest
       case x of
         Left s -> ircPrivmsg source ("cannot delete invalid repository: " ++ s)
         Right r -> withRepos $ \repos setRepos -> do
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

mkRepo :: String -> LB (Either String Repo)
mkRepo pref_ =
    do x <- liftIO $ do let path = mkInventoryPath pref_
                            pref = dropSpace pref_
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

watchRepos :: DPW ()
watchRepos =
    do withRepos $ \repos setRepos ->
           do debug ("checking darcs repositories " ++ showRepos repos)
              repos_ <- mapM checkRepo repos
              setRepos repos_
       liftIO $ threadDelay sleepTime
       watchRepos
    where sleepTime :: Int  -- in milliseconds
          sleepTime = checkInterval * 1000 * 1000

-- actually work out if we need to send a message
--
checkRepo :: Repo -> LB Repo
checkRepo r = do
       (output, errput) <- liftIO $ runDarcs (repo_location r)
       nlines <-
         if not (null errput)
            then do info ("\ndarcs failed: " ++ errput)
                    return (repo_nlinesAtLastAnnouncement r)
            else do let olines = lines output
                        lastN = repo_nlinesAtLastAnnouncement r
                        new = take (length olines - lastN) olines
                    when (not (null new)) $
                        send $ mkMsg (repo_location r) (parseDarcsMsg (unlines new))
                    return (length olines)

       now <- liftIO getClockTime
       ct <- liftIO $ toCalendarTime now
       return $ r { repo_nlinesAtLastAnnouncement = nlines
                  , repo_lastAnnounced = Just ct }
    where
        send s = ircPrivmsg announceTarget s

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
             then liftIO (putStrLn ("[DarcsPatchWatch] " ++ s))
             else return ()

info :: MonadIO m => String -> m ()
info s = liftIO $ putStrLn ("[DarcsPatchWatch] " ++ s)

formatTime :: CalendarTime -> String
formatTime = calendarTimeToString

