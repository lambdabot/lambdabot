module Main where

import Char

main = do modules <- readFile "StaticModules.conf"
          writeFile "StaticModules.hs" $ unlines $ process $ lines modules

process modules = process' (filter isValid modules)

isValid [] = False
isValid ('#':_) = False
isValid _ = True

process' modules = begin ++ map doimport modules 
                ++ middle ++ map doload modules

begin = ["module StaticModules (loadStaticModules) where",
         "import IRC",
         ""]

doimport name = "import " ++ upperise name ++ "Module"

upperise (c:cs) = toUpper c:cs

middle = ["",
          "loadStaticModules :: LB ()",
          "loadStaticModules",
          " = do"]

doload name = "        ircInstallModule " ++ name ++ "Module"
