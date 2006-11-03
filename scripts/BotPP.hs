{-# OPTIONS -O -funbox-strict-fields #-}
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

--
-- Implements a preprocessor for plugins, filling in commonly required
-- syntax.
--
-- Currently only useful for plugins that only:
--  import Plugin
-- and have () for state.  
--
-- Also used to generate the modules list in Modules.hs
--

import System.Environment
import Data.Char
import Data.List

import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B -- crank it up, yeah!

main = do
    -- putStr "BotPP called with args: ";  print =<< getArgs
    [orig,i,o] <- getArgs
    let basename = baseName orig
    -- putStr "basename = "; print basename
    B.readFile i >>= \l -> B.writeFile o $ expand (B.length l) 0 basename l

baseName :: FilePath -> FilePath
baseName s = base 
    where
    rfile = takeWhile (not . (`elem` "/\\")) (reverse s)
    base  = takeWhile (/='.') (reverse rfile)

--
-- tricksy non-copying implementation.
--
expand :: Int -> Int -> String -> ByteString -> ByteString
expand l n basename xs
    | n `seq` xs `seq` False               = undefined -- strict
    | l == n                               = xs        -- not found
    | pLUGIN  `B.isPrefixOf` (B.drop n xs) = B.concat $ pref : render  name  ++ [B.tail rest]
    | mODULES `B.isPrefixOf` (B.drop n xs) = B.concat $ pref : modules basename name' ++ [B.tail rest']
    | otherwise                            = expand l (n+1) basename xs

    where pref         = B.take n xs    -- accumulating an offset

          pLUGIN       = pack "PLUGIN "
          (name, rest) = B.break (=='\n') (B.drop (n+B.length pLUGIN) xs)

          mODULES      = pack "MODULES "
          (name', rest') = B.break (=='\n') (B.drop (n+B.length mODULES) xs)

--
-- render the plugin boiler plate
--
render :: ByteString -> [ByteString]
render name =
    pack "newtype "  : name :
    pack "Module = " : name : pack "Module () \n\ 
    \\n\ 
    \theModule :: MODULE                            \n\ 
    \theModule = MODULE $ " : name : pack "Module ()       \n" : []

--
-- Collect all the plugin names, and generate the Modules.hs file
--
modules :: String -> ByteString -> [ByteString]
modules basename s =
    [pack "module ", pack basename, pack " (modulesInfo) where\n"
    ,pack "import Lambdabot\n"
    ,pack "\n"]
    ++ (concatMap importify statics) ++
    [pack "modulesInfo :: (LB (), [String])\n"
    ,pack "modulesInfo = (loadStaticModules, plugins)\n"
    ,pack "loadStaticModules :: LB ()\n"
    ,pack "loadStaticModules = do\n"]
    ++ (concatMap instalify statics) ++
    [pack "plugins :: [String]\n"
    ,pack "plugins = ["]
    ++ ((concat . (intersperse [pack ","]) . map pluginify) plugins) ++
    [pack "]\n"]

    where ms  = filter nonWhitespace $ B.split ' ' s

          statics = sort statics'
          plugins = sort plugins'

          (statics', rest) = break ((==)(pack ":")) ms
          plugins' = drop 1 rest

          importify x = [pack "import qualified Plugin.", x, B.singleton '\n']
          instalify x = [pack "  ircInstallModule Plugin.",x
                        ,pack ".theModule \""
                        ,B.map toLower x
                        , pack "\"\n"]
          pluginify x = [pack "\""
                        ,B.map toLower x
                        ,pack "\""]
                               

          nonWhitespace s 
              | s == pack ""   = False
              | s == pack "\n" = False
              | otherwise      = True
