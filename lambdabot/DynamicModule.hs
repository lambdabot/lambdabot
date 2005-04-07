--
-- dynamic : interface to the runtime loader
--

module DynamicModule (DynamicModule, dynamicModule) where

import IRC
import BotConfig
import RuntimeLoader
import ErrorUtils
import Util

import Map (Map)
import qualified Map as M hiding (Map)

import Data.Dynamic (Typeable)
import Data.Dynamic (fromDynamic)
import Data.IORef
import Data.Maybe
import Data.Set

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Exception (Exception (..))

import System.IO           ( stdout, hFlush )

------------------------------------------------------------------------

newtype DynamicModule = DynamicModule ()

dynamicModule :: DynamicModule
dynamicModule = DynamicModule ()

data DLModules 
 = DLModules 
    { objects :: Map String (RuntimeModule,Int)  -- object + use count
    , packages :: Set String -- which have been loaded? 
                             --  (can't unload, so ref counting pointless)
    }
  deriving Typeable

type Dyn a = MonadLB m => ModuleT DLModules m a

#if __GLASGOW_HASKELL__ < 603
empty :: Set a
empty = emptySet
#endif

initDLModules :: DLModules
initDLModules = DLModules { objects = M.empty, packages = empty }

instance Module DynamicModule DLModules where
  moduleName   _ = return "dynamic"
  moduleHelp _ _ = return "@dynamic-(un|re)?load: interface to dynamic linker"
  moduleSticky _ = True
  commands     _ = return ["dynamic-load","dynamic-unload","dynamic-reload"]
  moduleInit   _ = do liftIO initialise
                      writeMS initDLModules
                      startupModules <- getStartupModules
                      mapM_ (handleRLEConsole . load) startupModules
                                    
  process _ msg src "dynamic-load" rest 
    = checkPrivs msg src $ handleRLE src $ do load rest
                                              ircPrivmsg src "module loaded"

  process _ msg src "dynamic-unload" rest
    = checkPrivs msg src $ handleRLE src $ do unload rest
                                              ircPrivmsg src "module unloaded"

  process _ msg src "dynamic-reload" rest
    = checkPrivs msg src $ handleRLE src $ do unload rest
                                              load rest
                                              ircPrivmsg src "module reloaded"

  process _ _ _ _ _ = error "DynamicModule: Invalid command"


handleRLEConsole :: MonadLB m => m () -> m ()
handleRLEConsole = handleErrorJust findRLEError (liftIO . print)

handleRLE :: MonadIRC m => String -> m () -> m ()
handleRLE src = handleErrorJust findRLEError (ircPrivmsg src . show)

findRLEError :: IRCError -> Maybe RuntimeLoaderException
findRLEError (IRCRaised (DynException e)) = fromDynamic e
findRLEError _ = Nothing

data DLAccessor m
 = DLAccessor { objectsA  :: Accessor m (Map String (RuntimeModule,Int))
              , packagesA :: Accessor m (Set String)
              }

dlGet :: (MonadReader (IORef DLModules) m, MonadLB m) => m (DLAccessor m)
dlGet = do dlFMRef <- ask
           let dlA = dlAccessor dlFMRef
           return $ DLAccessor { objectsA  = objectsAccessor dlA
                               , packagesA = packagesAccessor dlA
                               }

dlAccessor :: MonadIO m => IORef DLModules -> Accessor m DLModules
dlAccessor ref 
 = Accessor { reader = liftIO $ readIORef ref,
              writer = liftIO . writeIORef ref }

objectsAccessor :: MonadIO m 
                => Accessor m DLModules
                -> Accessor m (Map String (RuntimeModule,Int))
objectsAccessor a
  = Accessor { reader = liftM objects (reader a)
             , writer = \v -> do s <- reader a
                                 writer a (s { objects = v })
             }

packagesAccessor :: MonadIO m 
                 => Accessor m DLModules 
                 -> Accessor m (Set String)
packagesAccessor a
  = Accessor { reader = liftM packages (reader a)
             , writer = \v -> do s <- reader a
                                 writer a (s { packages = v })
             }

load :: String -> Dyn ()
load name
     = do 
          file <- getModuleFile name
          alreadyloaded <- isLoadedObject file
          when alreadyloaded $ error "already loaded"
          object <- doLoadObject file
          catchError 
           (do liftIO $ resolveFunctions
               MODULE md <- liftIO $ loadFunction object "theModule"
               liftLB $ ircInstallModule md)
           (\e -> do doUnloadObject file ; throwError e)

unload :: String -> Dyn ()
unload name
     = do 
          liftLB $ ircUnloadModule name
          file <- getModuleFile name
          doUnloadObject file

doLoadRequire :: Require -> Dyn ()
doLoadRequire (Object file) = doLoadObject file >> return ()
doLoadRequire (Package name) = doLoadPackage name

doLoadObject :: String -> Dyn RuntimeModule
doLoadObject file
 = do dl <- dlGet
      requires <- getFileRequires file
      mapM_ doLoadRequire requires      
      loaded <- readFM (objectsA dl) file
      case loaded of
        Nothing -> do object <- liftIO $ loadObject file
                      writeFM (objectsA dl) file (object,1)
                      return object
        Just (object,n) -> do writeFM (objectsA dl) file (object,n+1)
                              return object

doLoadPackage :: String -> Dyn ()
doLoadPackage name
 = do dl <- dlGet
      loaded <- lookupSet (packagesA dl) name
      if not loaded then do liftIO $ loadPackage name
                            insertSet (packagesA dl) name
                else return ()

doUnloadRequire :: Require -> Dyn ()
doUnloadRequire (Object file) = doUnloadObject file
doUnloadRequire (Package name) = doUnloadPackage name

doUnloadPackage :: (Monad m) => t -> m ()
doUnloadPackage _ = return () -- can't unload packages

doUnloadObject :: String -> Dyn ()
doUnloadObject file
 = do dl <- dlGet
      loaded <- readFM (objectsA dl) file
      case loaded of
        Just (object,1)       -> do liftIO $ unloadObject object
                                    deleteFM (objectsA dl) file
        Just (object,n) | n>1 -> writeFM (objectsA dl) file (object,n-1)
        _                     -> error "DynamicModule: Nothing"
      requires <- getFileRequires file
      mapM_ doUnloadRequire requires      

isLoadedObject :: String -> Dyn Bool
isLoadedObject file
 = do dl <- dlGet
      loaded <- readFM (objectsA dl) file
      case loaded of
        Just _ -> return True
        Nothing -> return False

--
-- This stuff sucks. What follows are hard-coded package deps.
-- For a lambdabot compiled with a given GHC version, you can establish
-- the runtime deps sort of like so:
{-
        for i in *Module.hi ; do 
                ghc-6.4 --show-iface $i | sed -n '/package/{p;n;p;q;}' | sed 's/^[^:]*: //'
        done | tr ' ' '\n' | sort | uniq | xargs | \
                sed 's/-.\..//g;s/ /", "/g;s/^/["/;s/$/"]/'
-}
-- You may then have to edit, and reorder things a bit to get the load order correct :/
--
-- Still need "posix" until PosixCompat works more reliably.
--
initialise :: IO ()
initialise = do 
        initialiseRuntimeLoader
        putStr "Loading package " >> hFlush stdout
        mapM_ loadPackage
#if   __GLASGOW_HASKELL__ >= 604
          ["base", "haskell98", "mtl", "parsec", "network", "unix"]
#elif __GLASGOW_HASKELL__ >= 602
          ["base","haskell98","lang","parsec","network","unix","posix"]
#else /* hack */
          ["base", "haskell98", "parsec", "network", "unix", "posix"]
#endif
        putStrLn "... done."
        -- more hard coded evil
        mapM_ (\n -> loadObject (n++".o"))
              ["BotConfig","ErrorUtils","ExceptionError",
               "MonadException","Util","DeepSeq","Map","IRC","PosixCompat"]
                

{-
getModule :: FilePath -> IO MODULE
getModule name = do object <- loadObject name
                    resolveFunctions
                    loadFunction object "theModule" :: IO MODULE
-}
