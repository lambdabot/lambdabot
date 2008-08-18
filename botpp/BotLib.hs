module BotLib where


import Data.ByteString.Char8 (pack, ByteString)
import qualified Data.ByteString.Char8 as B -- crank it up, yeah!

import Data.Char
import Data.List


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


          nonWhitespace t
              | t == pack ""   = False
              | t == pack "\n" = False
              | otherwise      = True
