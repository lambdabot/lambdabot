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
    [_,i,o] <- getArgs
    B.readFile i >>= \l -> B.writeFile o $ expand (B.length l) 0 l

--
-- tricksy non-copying implementation.
--
expand :: Int -> Int -> ByteString -> ByteString
expand l n xs
    | n `seq` xs `seq` False               = undefined -- strict
    | l == n                               = xs        -- not found
    | pLUGIN  `B.isPrefixOf` (B.drop n xs) = B.concat $ pref : render  name  ++ [B.tail rest]
    | mODULES `B.isPrefixOf` (B.drop n xs) = B.concat $ pref : modules name' ++ [B.tail rest']
    | otherwise                            = expand l (n+1) xs

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
modules :: ByteString -> [ByteString]
modules s =
    [pack "module Modules where\n"
    ,pack "import Lambdabot\n"
    ,pack "\n"]
    ++ (concatMap importify ms) ++
    [pack "loadStaticModules :: LB ()\n"
    ,pack "loadStaticModules = do\n"]
    ++ (concatMap instalify ms) ++
    [pack "plugins :: [String]\n"
    ,pack "plugins = []\n"]

    where ms  = sort $ B.split ' ' s
          importify x = [pack "import qualified Plugin.", x, B.singleton '\n']
          instalify x = [pack "  ircInstallModule Plugin.",x
                        ,pack ".theModule \""
                        ,B.map toLower x
                        , pack "\"\n"]

