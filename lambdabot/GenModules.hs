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

import Text.PrettyPrint

outfile :: [Char]
outfile = "Modules.hs"

main :: IO ()
main = do argv    <- getArgs
          if argv /= ["--depends"] 
           -- write static/dynamic imports file
           then do
                let (plugins,statics) = breakcsv "," argv
                    src               = unlines (process statics) ++ process2 plugins
                writeFile outfile src

           -- generate a dependency tree. todo, do the sed fragment here too.
           else do 
                raw <- getContents      -- list of ghc --show-iface output

                let syn'      = parse raw
                    (_,ds,ps) = unzip3 syn'
                    packages  = nub . sort . foldr1 (++) $ ps
                    depends   = foldr1 intersect $ ds
                    syn       = map (\(a,b,c) -> (a, b \\  depends, c)) syn'
                        
                let cores = vcat [text "corePlugins :: [String]"
                                 ,text $ "corePlugins = " ++ show depends]

                let pkgs  = vcat [text "reqPackages :: [String]"
                                 ,text $ "reqPackages = " ++ show packages]

                let src =  "-- Generated via \"make dynamic-depends\"\n"
                              ++ "module Depends where"
                              ++ "\n\n"
                              ++ "data Require = Object String | Package String"
                              ++ "\n\n"
                              ++ render cores
                              ++ "\n\n"
                              ++ render pkgs
                              ++ "\n\n"
                              ++ "getFileRequires :: String -> [Require]"
                              ++ "\n\n"
                              ++ (render . vcat . map ppr $ syn)
                              ++ "\n\n"
                              ++ "getFileRequires _ = []"

                writeFile "Depends.hs" src

    where breakcsv p = (\(a,b) -> (a,tail b)) . (break (== p))

------------------------------------------------------------------------

parse :: String -> [(String, [String], [String])]
parse s = map parseModule (split "#\n" s)

parseModule :: String -> (String, [String], [String])
parseModule s =
        let (modname, bs) = breakOnGlue one s
            (_, cs)       = breakOnGlue "module dependencies: " bs
            ds            = drop (length two) cs
            (depends,es)  = breakOnGlue "package dependencies: " ds
            packages      = drop (length three) es

        in (modname ,cleanls depends, map dropVersion (cleanls packages))

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
              dropVersion ('-':c:cs) | isDigit c = []
              dropVersion (c:cs) = c : dropVersion cs

------------------------------------------------------------------------
--
-- For each module, generate a list of module and package dependencies.
--
-- Any modules that are shared by all modules, are considered core and
-- will get loaded in one go.
--
ppr :: (String, [String], t) -> Doc
ppr (_,[],_)     = empty
ppr (mod,deps,_) =
        hang (hcat [text "getFileRequires \"", text mod, text ".o\" = ["])
             4 ((vcat $ intersperse comma $ 
                        map (\s -> hcat ([text "Object \"", 
                                         text (nodot s), 
                                         text ".o\""])) deps)
               $$ char ']')

   where
       nodot []       = []
       nodot ('.':cs) = '/':nodot cs
       nodot (c:cs)   = c  :nodot cs

------------------------------------------------------------------------

process :: [String] -> [String]
process m = concat [begin, 
                    map doimport m, 
                    middle, 
                    map doload m]
 where
    begin        = ["module Modules where", "import IRC", ""]
    doimport name= "import qualified " ++ (clean . upperise) name ++ "Module"
    middle       = ["","loadStaticModules :: LB ()","loadStaticModules"," = do"]
    doload name  = " ircInstallModule " ++ (clean . upperise) name ++ 
                     "Module.theModule"

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

