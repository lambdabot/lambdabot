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
import qualified Data.ByteString.Char8 as B -- crank it up, yeah!
import Data.ByteString.Char8 (pack, append, cons, ByteString)
import Data.ByteString.Base ( unsafeTake)

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
          (name, rest) = B.breakChar '\n' (B.drop (n+B.length pLUGIN) xs)
          (name, rest) = B.breakChar '\n' (B.drop (n+B.length pLUGIN) xs)
          pLUGIN       = pack "PLUGIN "
          mODULES      = pack "MODULES "

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
-- collect all the plugin names
--
-- expandModules [] = []
-- expandModules xs =
--     let (,) = break (== ' ') xs

