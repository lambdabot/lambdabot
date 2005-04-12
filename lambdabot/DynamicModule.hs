--
-- dynamic : interface to the runtime loader
--

module DynamicModule (theModule) where

import {-# SOURCE #-} Modules   (plugins)

import Config
import IRC
import RuntimeLoader
import ErrorUtils               (handleErrorJust)
import Util

import Map (Map)
import qualified Map as M       (empty)

import Data.Char                (toUpper)
import Data.Dynamic             (Typeable,fromDynamic)
import Data.IORef               (readIORef, writeIORef, IORef)
import Data.Set

import Control.Monad.Error
import Control.Monad.Reader
import Control.Exception        (Exception(DynException))

import System.IO                (stdout, hFlush)

------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 603
empty :: Set a
empty = emptySet
#endif

dynamicModule :: DynamicModule
dynamicModule = DynamicModule ()

theModule :: MODULE
theModule = MODULE dynamicModule

--
-- keep track of loaded modules
--
data DLModules 
 = DLModules 
    { objects  :: Map String (RuntimeModule,Int)  -- object + use count
    , packages :: Set String    -- which have been loaded? (can't unload)
    , depends  :: DepList
    }
  deriving Typeable

initDLModules :: DLModules
initDLModules = DLModules { objects = M.empty, packages = empty, depends = [] }

type Dyn a = MonadLB m => ModuleT DLModules m a

depfile :: String
depfile = "Depends.conf" -- created at build time

------------------------------------------------------------------------

instance Module DynamicModule DLModules where
  moduleName   _ = "dynamic"
  moduleHelp _ _ = return "@dynamic-(un|re)?load: interface to dynamic linker"
  moduleSticky _ = True
  moduleHelp _ _ = return "@dynamic-(un|re)?load <module>, interface to dynamic linker"
  moduleCmds   _ = return ["dynamic-load","dynamic-unload","dynamic-reload"]
 
  moduleInit   _ = do 
        (ds :: Depends) <- liftIO (readFile depfile >>= readM)
        liftIO $ initialise (reqPkgs ds) (reqObjs ds) 
        writeMS (initDLModules { depends = depList ds })
        liftIO $ putStr "Loading plugins\t" >> hFlush stdout
        mapM_ (handleRLEConsole . load) plugins
        liftIO $ putStrLn "... done."
                                    
  process _ msg src "dynamic-load" rest 
    = checkPrivs msg src $ handleRLE src $
            load rest >> ircPrivmsg src "module loaded"

  process _ msg src "dynamic-unload" rest
    = checkPrivs msg src $ handleRLE src $ 
            unload rest >> ircPrivmsg src "module unloaded"

  process _ msg src "dynamic-reload" rest
    = checkPrivs msg src $ handleRLE src $ 
            unload rest >> load rest >> ircPrivmsg src "module reloaded"

  process _ _ _ _ _ = error "DynamicModule: Invalid command"

------------------------------------------------------------------------

handleRLEConsole :: MonadLB m => m () -> m ()
handleRLEConsole = handleErrorJust findRLEError (liftIO . print)

handleRLE :: String -> IRC () -> IRC ()
handleRLE src = handleErrorJust findRLEError (ircPrivmsg src . show)

findRLEError :: IRCError -> Maybe RuntimeLoaderException
findRLEError (IRCRaised (DynException e)) = fromDynamic e
findRLEError _                            = Nothing

-- ---------------------------------------------------------------------
-- Getting at the state
--

data DLAccessor m
 = DLAccessor { objectsA  :: Accessor m (Map String (RuntimeModule,Int))
              , packagesA :: Accessor m (Set String)
              , dependsA  :: Accessor m (DepList)
              }

dlGet :: (MonadLB m) => ModuleT DLModules m (DLAccessor m)
dlGet = do let dlFMRef = ?ref
           let dlA = dlAccessor dlFMRef
           return $ DLAccessor { objectsA  = objectsAccessor dlA
                               , packagesA = packagesAccessor dlA
                               , dependsA  = dependsAccessor dlA
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

dependsAccessor :: MonadIO m => Accessor m DLModules -> Accessor m DepList
dependsAccessor a
  = Accessor { reader = liftM depends (reader a)
             , writer = \v -> do s <- reader a
                                 writer a (s { depends = v })
             }

-- ---------------------------------------------------------------------
--
-- simple name of a plugin, i.e. "google"
-- load value "theModule" from each plugin.
--
load :: String -> Dyn ()
load nm = do 
        let file = getModuleFile nm
        alreadyloaded <- isLoadedObject file
        when alreadyloaded $ error "already loaded"
        object <- doLoadObject file
        catchError (do liftIO $ resolveFunctions
                       md <- liftIO $ loadFunction object "theModule"
                       liftLB $ ircInstallModule md nm)
                   (\e -> doUnloadObject file >> throwError e)

unload :: String -> Dyn ()
unload nm = do 
        unless (nm `elem` plugins) $ error "unknown/static module"
        liftLB $ ircUnloadModule nm
        doUnloadObject (getModuleFile nm)

doLoadObject :: String -> Dyn RuntimeModule
doLoadObject file = do 
        dl <- dlGet
        requires <- lookupList (dependsA dl) file
        mapM_ doLoadObject requires
        loaded <- readFM (objectsA dl) file
        case loaded of
                Nothing -> do 
                        object <- liftIO $ loadObjFile file
                        writeFM (objectsA dl) file (object,1)
                        return object
                Just (object,n) -> do 
                        writeFM (objectsA dl) file (object,n+1)
                        return object

{-
-- We do no post-init dynamic loading of packages. Is this that bad?
doLoadPackage :: String -> Dyn ()
doLoadPackage nm = do 
      dl <- dlGet
      loaded <- lookupSet (packagesA dl) nm
      when (not loaded) $ do 
            liftIO $ loadPackage nm
            insertSet (packagesA dl) nm
-}

doUnloadObject :: String -> Dyn ()
doUnloadObject file
 = do dl <- dlGet
      loaded <- readFM (objectsA dl) file
      case loaded of
        Just (object,1)       -> do liftIO $ unloadObject object
                                    deleteFM (objectsA dl) file
        Just (object,n) | n>1 -> writeFM (objectsA dl) file (object,n-1)
        _                     -> error "DynamicModule: Nothing"
      requires <- lookupList (dependsA dl) file
      mapM_ doUnloadObject requires

isLoadedObject :: String -> Dyn Bool
isLoadedObject file
 = do dl <- dlGet
      loaded <- readFM (objectsA dl) file
      case loaded of
        Just _ -> return True
        Nothing -> return False

-- ---------------------------------------------------------------------
--
-- | Load in required packages and objects. Arguments are list of
-- packages and core modules to load, extracted from Depends.conf,
-- produced by GenModules, via ghc --show-iface. Phew!
--
initialise :: [String] -> [String] -> IO ()
initialise pkgs objs = do 
        initialiseRuntimeLoader

        putStr "Loading package " >> hFlush stdout
        mapM_ loadPackage pkgs
        putStrLn "... done."

        putStr "Loading core\t" >> hFlush stdout
        mapM_ (\n -> loadObjFile (n++".o")) objs
        putStrLn "... done."
                
getModuleFile :: [Char] -> [Char]
getModuleFile s = upperise s ++ "Module.o"
    where
        upperise :: [Char] -> [Char]
        upperise []     = []
        upperise (c:cs) = toUpper c:cs

