--
-- Preprocessor for setting which modules are statically linked
--
module Main where

import Char

main :: IO ()
main = do modules <- readFile "StaticModules.conf"
          writeFile "StaticModules.hs" $ unlines $ process $ lines modules

process :: [[Char]] -> [[Char]]
process modules = process' (filter isValid modules)
    where
        isValid :: [Char] -> Bool
        isValid [] = False
        isValid ('#':_) = False
        isValid _ = True

        process' :: [[Char]] -> [[Char]]
        process' modules = concat [begin, map doimport modules, 
                                   middle, map doload modules]

begin :: [[Char]]
begin = ["module StaticModules (loadStaticModules) where",
         "import IRC",
         ""]

doimport :: [Char] -> [Char]
doimport name = "import " ++ (clean . upperise) name ++ "Module"

upperise :: [Char] -> [Char]
upperise (c:cs) = toUpper c:cs

clean :: [Char] -> [Char]
clean = reverse . dropWhile (== ' ') . reverse

middle :: [[Char]]
middle = ["",
          "loadStaticModules :: LB ()",
          "loadStaticModules",
          " = do"]

doload :: [Char] -> [Char]
doload name = " ircInstallModule " ++ name ++ "Module"
