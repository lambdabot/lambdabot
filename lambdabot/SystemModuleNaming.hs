{-# OPTIONS -fno-warn-unused-binds #-}

#include "config.h"

module SystemModuleNaming (systemModuleName) where

import Monad
import Directory
import qualified Control.Exception as C (catch,throw)

--
-- We're trying to get a few fields from the package.conf files. How to
-- do this differs depending on post-6.4 and pre-6.4 ghc.
--

#if __GLASGOW_HASKELL__  >= 604
import Distribution.InstalledPackageInfo
import Distribution.Package

-- What type is the stuff in package.conf files?
type PkgConf = InstalledPackageInfo

--
-- we get accessor functions from our Package type, now, let's map those
-- to Cabal field names.
--
name :: PkgConf -> String
name = pkgName . package

library_dirs :: PkgConf -> [String]
library_dirs = libraryDirs

hs_libraries :: PkgConf -> [String]
hs_libraries = hsLibraries

extra_libraries :: PkgConf -> [String]
extra_libraries = extraLibraries

#else /* GHC < 6.4 */
--
-- This is a pre-6.4 package.conf file. We need to define our own type
-- for it.
--
type PkgConf = Package

data Package
 = Package { name               :: !String
           , auto               :: Bool
           , import_dirs        :: [String]
           , source_dirs        :: [String]
           , library_dirs       :: ![String]
           , hs_libraries       :: ![String]
           , extra_libraries    :: ![String]
           , include_dirs       :: [String]
           , c_includes         :: [String]
           , package_deps       :: [String]
           , extra_ghc_opts     :: [String]
           , extra_cc_opts      :: [String]
           , extra_ld_opts      :: [String]
           , framework_dirs     :: [String]
           , extra_frameworks   :: [String]
           }
  deriving Read
#endif

-- cpp'd:
ghcLibraryPath :: String
ghcLibraryPath = GHC_LIB_PATH ++ "/"

--
-- Given a package name, e.g. 'concurrent', find the full path to that object
--
systemModuleName :: String -> IO [String]
systemModuleName packageName = do

   (packages :: [PkgConf]) <- C.catch
         (liftM read $ readFile $ ghcLibraryPath ++ "package.conf")
         (\e -> putStrLn "Unable to read package.conf" >> C.throw e)

   let pkg = head (filter (\p -> name p == packageName) packages)
   liftM concat $ sequence $
        map (libraryObject (map translate (library_dirs pkg)))
            (hs_libraries pkg ++ extra_libraries pkg)

translate :: [Char] -> [Char]
translate ('$':'l':'i':'b':'d':'i':'r':rest) = init ghcLibraryPath ++ rest
translate other = other

libraryObject :: [[Char]] -> [Char] -> IO [[Char]]
libraryObject [] _ = return []
libraryObject (dir:dirs) nm = do
	let filename = dir ++ "/" ++ nm ++ ".o"
	exists <- doesFileExist filename
	if exists then return [filename] else libraryObject dirs nm

