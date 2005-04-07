--
-- dynamic : interface to the runtime loader
--

module DynamicModule (DynamicModule, dynamicModule) where

import IRC
import Config
import RuntimeLoader
import ErrorUtils
import Util

import {-# SOURCE #-} Modules                  (plugins)

import Map (Map)
import qualified Map as M hiding (Map)

import Data.Char                (toUpper)
import Data.Dynamic             (Typeable,fromDynamic)
import Data.IORef
import Data.Maybe
import Data.Set

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Exception        (Exception (..))

import System.IO                (stdout, hFlush)

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
  moduleInit   _ = do 
        liftIO initialise
        writeMS initDLModules
        liftIO $ putStr "Loading plugins\t" >> hFlush stdout
        mapM_ (handleRLEConsole . load) plugins
        liftIO $ putStrLn "... done."
                                    
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
findRLEError _                            = Nothing

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
                       MODULE md <- liftIO $ loadFunction object "theModule"
                       liftLB $ ircInstallModule md)
                   (\e -> doUnloadObject file >> throwError e)

unload :: String -> Dyn ()
unload nm = do liftLB $ ircUnloadModule nm
               doUnloadObject (getModuleFile nm)

doLoadRequire :: Require -> Dyn ()
doLoadRequire (Object file) = doLoadObject file >> return ()
doLoadRequire (Package nm)  = doLoadPackage nm

doLoadObject :: String -> Dyn RuntimeModule
doLoadObject file = do 
        dl <- dlGet
        let requires = getFileRequires file
        mapM_ doLoadRequire requires      
        loaded <- readFM (objectsA dl) file
        case loaded of
                Nothing -> do 
                        object <- liftIO $ loadObjFile file
                        writeFM (objectsA dl) file (object,1)
                        return object
                Just (object,n) -> do 
                        writeFM (objectsA dl) file (object,n+1)
                        return object

doLoadPackage :: String -> Dyn ()
doLoadPackage nm = do 
      dl <- dlGet
      loaded <- lookupSet (packagesA dl) nm
      when (not loaded) $ do 
            liftIO $ loadPackage nm
            insertSet (packagesA dl) nm

doUnloadRequire :: Require -> Dyn ()
doUnloadRequire (Object file) = doUnloadObject file
doUnloadRequire (Package nm)  = doUnloadPackage nm

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
      mapM_ doUnloadRequire (getFileRequires file)

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

        -- more hard coded evil. Abolish!
        putStr "Loading core\t" >> hFlush stdout
        mapM_ (\n -> loadObjFile (n++".o"))
              ["Config","ErrorUtils","ExceptionError",
               "MonadException","Util","DeepSeq","Map","IRC","PosixCompat"]
        putStrLn "... done."
                
getModuleFile :: [Char] -> [Char]
getModuleFile s = upperise s ++ "Module.o"
    where
        upperise :: [Char] -> [Char]
        upperise []     = []
        upperise (c:cs) = toUpper c:cs

{-
getModule :: FilePath -> IO MODULE
getModule name = do object <- loadObject name
                    resolveFunctions
                    loadFunction object "theModule" :: IO MODULE
-}
