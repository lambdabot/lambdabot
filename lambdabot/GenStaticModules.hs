--
-- Preprocessor for setting which modules are statically linked
--
module Main where

import Char

main :: IO ()
main = do contents <- readFile "StaticModules.conf"
          let modules = map clean . filter isValid $ lines contents
          writeFile "StaticModules.hs" $ unlines $ process modules
          writeFile "StaticModules2.hs" $ unlines $ process2 modules

isValid :: String -> Bool
isValid [] = False
isValid ('#':_) = False
isValid _ = True

clean :: String -> String
clean = reverse . dropWhile (== ' ') . reverse

process :: [String] -> [String]
process modules = concat [begin, map doimport modules, 
                          middle, map doload modules] where
  begin :: [String]
  begin = ["module StaticModules (loadStaticModules) where",
           "import IRC",
           ""]

  doimport :: String -> String
  doimport name = "import " ++ (clean . upperise) name ++ "Module"

  upperise :: String -> String
  upperise (c:cs) = toUpper c:cs
  upperise []     = []

  middle :: [String]
  middle = ["",
            "loadStaticModules :: LB ()",
            "loadStaticModules",
            " = do"]

  doload :: String -> String
  doload name = " ircInstallModule " ++ name ++ "Module"

process2 :: [String] -> [String]
process2 modules = concat [begin, map doYes modules, end] where
  begin = ["module StaticModules2 (isStaticModule) where",
           "",
           "isStaticModule :: String -> Bool"]
  doYes name = "isStaticModule " ++ show name ++ " = True"
  end = ["isStaticModule _ = False"]
