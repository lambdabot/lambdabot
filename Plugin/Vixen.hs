--
-- | Talk to hot chixxors.
--
-- (c) Mark Wotton
--
module Plugin.Vixen where

import Plugin.Vixen.VixenState
import Plugin hiding (Regex, matchRegex)
import Lib.Regex
import qualified Data.ByteString.Char8 as P

PLUGIN Vixen

instance Module VixenModule (Bool, String -> IO String) where
    moduleCmds   _   = ["vixen"]
    modulePrivs  _   = ["vixen-on", "vixen-off"] -- could be noisy
    moduleHelp _ _   = "vixen <phrase>. Sergeant Curry's lonely hearts club"
    moduleDefState _ = return $ (False, mkVixen)

    -- if vixen-chat is on, we can just respond to anything
    contextual _ _ _ txt      = do (alive, k) <- readMS
                                   if alive then ios (k txt) else return []

    process_ _ "vixen-on"  _ = withMS $ \(_,r) k -> k (True, r)  >> return ["What's this channel about?"]
    process_ _ "vixen-off" _ = withMS $ \(_,r) k -> k (False, r) >> return ["Bye!"]
    process_ _ _ txt         = readMS >>= ios . ($ txt) . snd

mkVixen :: String -> IO String
mkVixen question = vixen (mkResponses state) question

-- use IO only for random, could remove it.
vixen :: (String -> WTree) -> String -> IO String
vixen responder them = do x <- randomWTreeElt (responder them)
                          return (P.unpack x)

randomWTreeElt :: WTree -> IO P.ByteString
randomWTreeElt (Leaf a)  = return a
randomWTreeElt (Node ls) = stdGetRandItem ls >>= randomWTreeElt

match :: Regex -> String -> Bool
match r s = matchRegex r s

mkResponses :: RespChoice -> String -> WTree
mkResponses choices them =
    (\((_,wtree):_) -> wtree) $ filter (\(reg,_) -> match reg them) choices

------------------------------------------------------------------------
