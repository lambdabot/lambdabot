{-# OPTIONS -fglasgow-exts #-}

module SystemModuleNaming (systemModuleName) where

import GHCLibraryPath (ghcLibraryPath)
import Monad
import Directory

data Package 
 = Package { name               :: String
           , auto               :: Bool
           , import_dirs        :: [String]
           , source_dirs        :: [String]
           , library_dirs       :: [String]
           , hs_libraries       :: [String]
           , extra_libraries    :: [String]
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


systemModuleName :: String -> IO [String]
systemModuleName packageName
 = do (packages :: [Package])
           <- liftM read $ readFile (ghcLibraryPath ++ "package.conf")
      let package = head (filter (\p -> name p == packageName) packages)
      liftM concat $ sequence $ 
         map (libraryObject (map translate (library_dirs package)))
             (hs_libraries package ++ extra_libraries package)


translate ('$':'l':'i':'b':'d':'i':'r':rest) = init ghcLibraryPath ++ rest
translate other = other

libraryObject [] name = return []
libraryObject (dir:dirs) name 
 = do let filename = dir ++ "/" ++ name ++ ".o"
      exists <- doesFileExist filename
      if exists then return [filename]
                else libraryObject dirs name



