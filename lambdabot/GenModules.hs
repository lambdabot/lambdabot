--
-- Preprocessor for setting which modules are statically linked
--
-- Generates a list of dynamic modules to load, and a static module with
-- appropriate (static) imports. The input file is generated from the
-- $(PLUGINS) and $(STATICS) vars passed on the command line via
-- Makefile, from config.mk
--
-- TODO, would be nice to use a small, stable subset of TH to make this.
--
module Main where

import Char
import List                     ( isPrefixOf, intersperse )
import System.Environment

outfile = "Modules.hs"

main :: IO ()
main = do argv    <- getArgs
          let (plugins,statics) = (\(a,b) -> (a,tail b)) (break (== ",") argv)
              src               = unlines (process statics) ++ process2 plugins
          writeFile outfile src

process :: [String] -> [String]
process m = concat [begin, 
                    map doimport m, 
                    middle, 
                    map doload m]
 where
    begin        = ["module Modules where", "import IRC", ""]
    doimport name= "import " ++ (clean . upperise) name ++ "Module"
    middle       = ["","loadStaticModules :: LB ()","loadStaticModules"," = do"]
    doload name  = " ircInstallModule " ++ (clean . lowerise) name ++ "Module"

process2 :: [String] -> String
process2 ms = "\nplugins :: [String]\n" ++ concat ("plugins = [" : 
                        intersperse ", " 
                           (map (quote.lowerise) ms)) ++ "]\n"

------------------------------------------------------------------------

quote  :: String -> String
quote x = "\"" ++ x ++ "\""

upperise :: [Char] -> [Char]
upperise (c:cs) = toUpper c:cs

lowerise :: [Char] -> [Char]
lowerise (c:cs) = toLower c:cs

clean :: [Char] -> [Char]
clean = reverse . dropWhile (== ' ') . reverse

split glue xs = split' xs
  where
    split' []  = []
    split' xs' = let (as, bs) = breakOnGlue glue xs'
                 in as : split' (dropGlue bs)
    dropGlue = drop (length glue)

breakOnGlue :: (Eq a) => [a] -> [a] -> ([a],[a])
breakOnGlue _ []                = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest    = ([], rest)
    | otherwise                 = (x:piece, rest') 
        where (piece, rest') = breakOnGlue glue xs
