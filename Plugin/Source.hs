--
-- Plugin.Source
-- Display source for specified identifiers
--
module Plugin.Source (theModule) where

import Plugin
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString)

PLUGIN Source

type Env = M.Map ByteString ByteString

instance Module SourceModule Env where
    moduleCmds _     = ["src"]
    moduleHelp _ _   = "src <id>. Display the implementation of a standard function"

    -- all the hard work is done to build the src map.
    -- uses a slighly custom Map format
    moduleSerialize _= Just . readOnly $ M.fromList . map pair . splat .  P.lines
        where
            pair (a:b) = (a, P.unlines b)
            splat []   = []
            splat s    = a : splat (tail b) where (a,b) = break P.null s

    process_ _ _ t = do
        env <- readMS
        return $ case M.lookup key env of
            _ | M.null env -> ["No source in the environment yet"]
            Nothing        -> ["Source for this function is not available."]
            Just s         -> [P.unpack s]
        where key = P.pack t
