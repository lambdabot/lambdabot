--
-- | Talk to hot chixxors.
--
-- (c) Mark Wotton
--
module Plugin.Vixen where

import Plugin.Vixen.VixenState
import Plugin hiding (Regex, matchRegex)
import Lib.Regex
import qualified Data.FastPackedString as P

PLUGIN Vixen

instance Module VixenModule (String -> IO String) where
    moduleCmds   _   = ["vixen"]
    moduleHelp _ _   = "vixen <phrase>. Sergeant Curry's lonely hearts club"
    moduleDefState _ = return $ mkVixen
    process_ _ _ rest = do responder <- readMS
                           ios (responder rest)

mkVixen :: String -> IO String
mkVixen question = vixen (mkResponses state) question

-- use IO only for random, could remove it.
vixen :: (String -> WTree) -> String -> IO String
vixen responder them = do x <- randomWTreeElt (responder them)
                          return (P.unpack x)

randomWTreeElt :: WTree -> IO P.FastString
randomWTreeElt (Leaf a)  = return a
randomWTreeElt (Node ls) = stdGetRandItem ls >>= randomWTreeElt

match :: Regex -> String -> Bool
match r s = matchRegex r s

mkResponses :: RespChoice -> String -> WTree
mkResponses choices them = 
    (\((_,wtree):_) -> wtree) $ filter (\(reg,_) -> match reg them) choices

------------------------------------------------------------------------
