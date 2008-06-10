-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Boot loader for lambdabot.
-- This is a small stub that loads the dynamic code, then jumps to it.
-- It solves the problem of unnecessary code linking.  As such, we only
-- want to do plugin-related stuff here.
--
-- We should have no static dependencies on any other part of lambdabot.
--
-- This is the only module that depends on -package plugins.
--
-- Compile with:
--      ghc -fglasgow-exts -package plugins -main-is Boot.main Boot.hs

module Boot ( main ) where

import qualified Shared as S

import System.Plugins.Load

import Data.Map (Map)
import qualified Data.Map    as M hiding (Map)
import Data.IORef
import System.IO.Unsafe       ( unsafePerformIO )
import System.Exit            ( exitFailure )

lambdabotMain :: [Char]
lambdabotMain = lambdaPath ++ "DMain.o" -- entry point into lambdabot lib

-- path to plugins
lambdaPath :: String
lambdaPath = "./dist/build/"

mainSym :: Symbol
mainSym  = "dynmain"        -- main entry point

-- | get a handle to Main.dynamic_main, and jump to it.
--
-- todo, link statically and dynamically, a module with a Module type we
-- can convert hs-plugins type to.
main :: IO ()
main = do
    status  <- load lambdabotMain [lambdaPath] [] mainSym
    dynmain <- case status of
        LoadSuccess _ v -> return (v :: MainType) -- should stick module in ioref
        LoadFailure e   -> do putStrLn "Unable to load Main, exiting"
                              mapM_ putStrLn e
                              exitFailure

    dynmain hsplugins

------------------------------------------------------------------------
-- Plugin.Loader wrappers passed over to the

type MainType = S.DynLoad -> IO ()

-- | should insert m into a local IORef, for unload to work
dLoad :: String -> String -> IO (S.Module,a)
dLoad p s = do
    mm <- load_ (lambdaPath ++ p) [lambdaPath] s
    case mm of
        LoadFailure e   -> error ("dLoad: "++show e)  -- throw this back to DynamicModule
        LoadSuccess m v -> do
            atomicModifyIORef modules $ \fm -> (M.insert (path m) m fm, ())
            return (S.Module (path m),v)

-- | Unload a module, given the path to that module
dUnload :: S.Module -> IO ()
dUnload (S.Module p) = do
    mm <- atomicModifyIORef modules $ \fm ->
        case M.lookup p fm of
            Nothing -> (fm,Nothing)    -- fail silently
            Just m  -> (M.delete p fm, Just m)
    case mm of
        Nothing -> return ()
        Just m  -> unload m

--
-- | Dynamic loader package we pass over.
--
hsplugins :: S.DynLoad
hsplugins = S.DynLoad {
        S.dynload    = dLoad,
        S.unload     = dUnload
    }

-- map module paths to hs-plugins' Module type. Saves us linking
-- hs-plugins statically and dynamically (code bloat)
modules :: IORef (Map FilePath Module)
modules = unsafePerformIO $ newIORef (M.empty)
{-# NOINLINE modules #-}
