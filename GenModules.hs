--
-- | Preprocessor for setting which modules are statically linked
--
-- Generates a list of dynamic modules to load, and a static module with
-- appropriate (static) imports. The input file is generated from the
-- $(PLUGINS) and $(STATICS) vars passed on the command line via
-- Makefile, from config.mk
--
module Main where

import qualified Util

import Data.Char
import Data.List

import System.Environment

outfile :: String
outfile  = "Modules.hs"
-- outfile' = "Depends.conf"

main :: IO ()
main = do argv <- getArgs
          if argv /= ["--depends"]
           -- write static/dynamic imports file
           then do
                let (plugins,statics) = breakCSV "," argv
                    src               = unlines (processStatics statics)
                                           ++ processDynamics plugins
                writeFile outfile src

           -- generate a dependency tree.
           -- must run after we've built the lambdabot.
           else error "Dynamic dependencies obsoleted by hs-plugins"

    where breakCSV p = (\(a,b) -> (a,tail b)) . (break (== p))

------------------------------------------------------------------------

--
-- there's an assumption that plugins are only capitalised in their
-- first letter.
--
processStatics :: [String] -> [String]
processStatics m = concat [begin,
			   map doimport m,
			   middle,
			   map doload m]
 where
    canon = (Util.dropSpace.)
    canonU = canon Util.upperize
    canonL = canon Util.lowerize

    begin        = ["module Modules where", "import Lambdabot", ""]
    doimport nm  = "import qualified Plugins." ++ canonU nm
    middle       = ["","loadStaticModules :: LB ()","loadStaticModules"," = do"]
    doload nm   = " ircInstallModule Plugins." ++ canonU nm  ++
                     ".theModule " ++ show (canonL $ nm)

processDynamics :: [String] -> String
processDynamics ms = "\nplugins :: [String]\n" ++ concat ("plugins = [" :
                        intersperse ", "
                           (map (Util.quote . Util.lowerize) ms)) ++ "]\n"

