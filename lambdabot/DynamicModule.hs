{-# OPTIONS -cpp -fglasgow-exts #-}

module DynamicModule (DynamicModule, dynamicModule) where

import Map (Map)
import qualified Map as M

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Control.Exception (Exception (..))
import Data.Dynamic (fromDynamic)
import Data.Set
import Data.IORef
import Data.Dynamic (Typeable)
import Maybe

import IRC
import BotConfig
import RuntimeLoader
import ErrorUtils
import Util

newtype DynamicModule = DynamicModule ()

dynamicModule = DynamicModule ()

data DLModules 
 = DLModules 
    { objects :: Map String (RuntimeModule,Int)  -- object + use count
    , packages :: Set String -- which have been loaded? 
                             --  (can't unload, so ref counting pointless)
    }
  deriving Typeable

#if __GLASGOW_HASKELL__ < 603
empty = emptySet
#endif

initDLModules = DLModules { objects = M.empty, packages = empty }

instance Module DynamicModule where
  moduleName   _ = return "dynamic"
  moduleSticky _ = True
  commands     _ = return ["dynamic-load","dynamic-unload","dynamic-reload"]
  moduleInit   _ = do liftIO $ initialise
                      makeInitialState "dynamic" initDLModules
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


handleRLEConsole = handleErrorJust findRLEError (liftIO . print)
handleRLE src = handleErrorJust findRLEError (ircPrivmsg src . show)

findRLEError :: IRCError -> Maybe RuntimeLoaderException
findRLEError (IRCRaised (DynException e)) = fromDynamic e
findRLEError _ = Nothing

data DLAccessor m
 = DLAccessor { objectsA  :: Accessor m (Map String (RuntimeModule,Int))
              , packagesA :: Accessor m (Set String)
              }

dlGet :: MonadLB m => m (DLAccessor m)
dlGet = do Just dlFMRef <- readFM ircModuleStateAccessor "dynamic"
           let dlA = dlAccessor dlFMRef
           return $ DLAccessor { objectsA  = objectsAccessor dlA
                               , packagesA = packagesAccessor dlA
                               }

dlAccessor :: MonadIO m => IORef ModuleState -> Accessor m DLModules
dlAccessor ref 
 = Accessor { reader = liftIO $ liftM stripMS $ readIORef ref,
              writer = \dl -> liftIO $ writeIORef ref (ModuleState dl) }

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

load :: MonadLB m => String -> m ()
load name
     = do 
          file <- getModuleFile name
          alreadyloaded <- isLoadedObject file
          when alreadyloaded $ error "already loaded"
          object <- doLoadObject file
          catchError 
           (do liftIO $ resolveFunctions
               MODULE mod <- liftIO $ loadFunction object "theModule"
               liftLB $ ircInstallModule mod)
           (\e -> do doUnloadObject file
                     throwError e)
unload name
     = do 
          dl <- dlGet
          liftLB $ ircUnloadModule name
          file <- getModuleFile name
          doUnloadObject file

doLoadRequire (Object file) = doLoadObject file >> return ()
doLoadRequire (Package name) = doLoadPackage name

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

doLoadPackage name
 = do dl <- dlGet
      loaded <- lookupSet (packagesA dl) name
      if not loaded then do liftIO $ loadPackage name
                            insertSet (packagesA dl) name
                else return ()

doUnloadRequire (Object file) = doUnloadObject file
doUnloadRequire (Package name) = doUnloadPackage name

doUnloadPackage _ = return () -- can't unload packages

doUnloadObject file
 = do dl <- dlGet
      loaded <- readFM (objectsA dl) file
      case loaded of
        Just (object,1) -> do liftIO $ unloadObject object
                              deleteFM (objectsA dl) file
        Just (object,n) | n>1 -> writeFM (objectsA dl) file (object,n-1)
      requires <- getFileRequires file
      mapM_ doUnloadRequire requires      

isLoadedObject file
 = do dl <- dlGet
      loaded <- readFM (objectsA dl) file
      case loaded of
        Just _ -> return True
        Nothing -> return False

initialise = do initialiseRuntimeLoader
                mapM_ loadPackage ["base","haskell98","lang","network",
#if __GLASGOW_HASKELL__ >= 600
                                   "unix",
#endif
                                   "posix"]
                mapM_ (\n -> loadObject (n++".o"))
                      ["BotConfig","ErrorUtils","ExceptionError",
                       "MonadException","Util","DeepSeq","IRC"]

getModule name = do object <- loadObject name
                    resolveFunctions
                    loadFunction object "theModule" :: IO MODULE
