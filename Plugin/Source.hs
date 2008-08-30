{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- Plugin.Source
-- Display source for specified identifiers
module Plugin.Source (theModule) where

import Plugin
import Lambdabot.Util
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (pack,ByteString)

$(plugin "Source")

type Env = M.Map ByteString ByteString

instance Module SourceModule Env where
    moduleCmds _     = ["src"]
    moduleHelp _ _   = help

    -- all the hard work is done to build the src map.
    -- uses a slighly custom Map format
    moduleSerialize _= Just . readOnly $ M.fromList . map pair . splat . P.lines
        where
            pair (a:b) = (a, P.unlines b)
            splat []   = []
            splat s    = a : splat (tail b) where (a,b) = break P.null s

    fprocess_ _ _ key = readMS >>= \env -> case fetch key env of
        _ | M.null env -> box $ pack "No source in the environment yet"
        _ | P.null key -> box $ pack help
        Nothing        -> box . P.pack . ("Source not found. " ++) =<< io (random insult)
        Just s         -> box s

fetch :: ByteString -> M.Map ByteString ByteString -> Maybe ByteString
fetch x m = M.lookup x m `mplus`
            M.lookup (P.concat [P.singleton '(', x, P.singleton ')']) m

help :: String
help = "src <id>. Display the implementation of a standard function"
