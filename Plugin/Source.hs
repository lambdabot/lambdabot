--
-- Plugin.Source
-- Display source for specified identifiers
--
module Plugin.Source (theModule) where

import Plugin
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (pack,ByteString)

PLUGIN Source

type Env = M.Map ByteString ByteString

instance Module SourceModule Env where
    moduleCmds _     = ["src"]
    moduleHelp _ _   = help

    -- all the hard work is done to build the src map.
    -- uses a slighly custom Map format
    moduleSerialize _= Just . readOnly $ M.fromList . map pair . splat . P.lines . gunzip
        where
            pair (a:b) = (a, P.unlines b)
            splat []   = []
            splat s    = a : splat (tail b) where (a,b) = break P.null s

    fprocess_ _ _ key = readMS >>= \env -> box $ case M.lookup key env of
        _ | M.null env -> pack "No source in the environment yet"
        _ | P.null key -> pack help
        Nothing        -> pack "Source for this function is not available."
        Just s         -> s

help :: String
help = "src <id>. Display the implementation of a standard function"
