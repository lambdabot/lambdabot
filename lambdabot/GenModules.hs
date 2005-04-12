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
import List
import System.Environment

outfile, outfile' :: String
outfile  = "Modules.hs"
outfile' = "Depends.conf"

main :: IO ()
main = do argv <- getArgs
          if argv /= ["--depends"]
           -- write static/dynamic imports file
           then do
                let (plugins,statics) = breakcsv "," argv
                    src               = unlines (process statics)
                                           ++ process2 plugins
                writeFile outfile src

           -- generate a dependency tree.
           -- must run after we've built the lambdabot.
           else do
                raw <- getContents      -- list of ghc --show-iface output

                let syn'      = parse raw
                    (_,ds,ps) = unzip3 syn'

                -- problem, doesn't take into account dependencies
                -- specified in package.confs. i.e. network->parsec
                -- should read ParsePkgConf.depends and add to list
                let pkgs' = (foldr1 (++) ps) \\ ["Cabal"]
                deppkgs <- findDepends pkgs'
                let pkgs   = (nub . foldr1 (++) $ deppkgs ++ [pkgs']) \\ ["rts"]
    
                let coremods  = foldr1 intersect $ ds
                    deplist   = map (\(a,b,_) -> 
                                        (a++".o"
                                        ,map (\s -> (nodot s)++".o") (b \\  coremods))) syn'

                let deps      = Depends { reqObjs = coremods,
                                          reqPkgs = pkgs,
                                          depList = deplist }

                writeFile outfile' (show deps)

    where breakcsv p = (\(a,b) -> (a,tail b)) . (break (== p))
          nodot []       = []
          nodot ('.':cs) = '/':nodot cs
          nodot (c:cs)   = c  :nodot cs

-- ---------------------------------------------------------------------
--
-- parse output of ghc --show-iface

parse :: String -> [(String, [String], [String])]
parse s = map parseModule (split "#\n" s)

parseModule :: String -> (String, [String], [String])
parseModule s =
        let (modname, bs) = breakOnGlue one s
            (_, cs)       = breakOnGlue "module dependencies: " bs
            ds            = drop (length two) cs
            (deps,es)     = breakOnGlue "package dependencies: " ds
            pkgs          = drop (length three) es

        in (modname ,cleanls deps, map dropVersion (cleanls pkgs))

        where one     = ".hi"
              two     = "module dependencies: "
              three   = "package dependencies: "
              cleanls = split " " . clean . squish

              -- strip \n, \t, and squish repeated ' '
              squish [] = []
              squish (c:cs) 
                | c == '\t'     = squish cs
                | c == '\n'     = squish cs
                | c == ' '      = c : squish (dropWhile (== ' ') cs)
                | otherwise     = c : squish cs

              -- drop cabal vers string fromm, e.g, "base-1.0"
              dropVersion [] = []
              dropVersion ('-':c:_) | isDigit c = []
              dropVersion (c:cs) = c : dropVersion cs

------------------------------------------------------------------------

process :: [String] -> [String]
process m = concat [begin, 
                    map doimport m, 
                    middle, 
                    map doload m]
 where
    begin        = ["module Modules where", "import IRC", ""]
    doimport nm  = "import qualified " ++ (clean . upperise) nm ++ "Module"
    middle       = ["","loadStaticModules :: LB ()","loadStaticModules"," = do"]
    doload nm   = " ircInstallModule " ++ (clean . upperise) nm  ++ 
                     "Module.theModule " ++ show (clean . upperise $ name)

process2 :: [String] -> String
process2 ms = "\nplugins :: [String]\n" ++ concat ("plugins = [" : 
                        intersperse ", " 
                           (map (quote.lowerise) ms)) ++ "]\n"

------------------------------------------------------------------------

quote  :: String -> String
quote x = "\"" ++ x ++ "\""

upperise :: [Char] -> [Char]
upperise [] = []
upperise (c:cs) = toUpper c:cs

lowerise :: [Char] -> [Char]
lowerise [] = []
lowerise (c:cs) = toLower c:cs

clean :: [Char] -> [Char]
clean = let f = reverse . dropWhile isSpace in f . f

split :: (Eq a) => [a] -> [a] -> [[a]]
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

