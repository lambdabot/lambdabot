--
-- | Talk to hot chixxors.
--
-- (c) Mark Wotton
-- Serialisation (c) 2007 Don Stewart
--
module Plugin.Vixen where

import Plugin hiding (Regex, matchRegex)
import Lib.Regex
import Lib.Binary

import Control.Arrow ((***))
import System.Directory
import qualified Data.ByteString.Char8 as P

PLUGIN Vixen

instance Module VixenModule (Bool, String -> IO String) where
    moduleCmds   _   = ["vixen"]
    modulePrivs  _   = ["vixen-on", "vixen-off"] -- could be noisy
    moduleHelp _ _   = "vixen <phrase>. Sergeant Curry's lonely hearts club"

    -- if vixen-chat is on, we can just respond to anything
    contextual _ _ _ txt      = do
        (alive, k) <- readMS
        if alive then ios (k txt)
                 else return []

    process_ _ "vixen-on"  _ =
        withMS $ \(_,r) k -> do k (True, r)
                                return ["What's this channel about?"]
    process_ _ "vixen-off" _ =
        withMS $ \(_,r) k -> do k (False, r)
                                return ["Bye!"]

    process_ _ _ txt = readMS >>= ios . ($ txt) . snd

    moduleDefState _ = return (False, const (return "<undefined>"))

    -- suck in our (read only) regex state from disk
    -- compile it, and stick it in the plugin state
    moduleInit _     = do
      b <- io $ doesFileExist file
      when b $ do
          s <- io $ do h  <- openFile file ReadMode
                       bh <- openBinIO_ h
                       st <- get bh :: IO Choice
                       hClose h
                       let compiled = map (regex *** id) st
                       return (vixen (mkResponses compiled))
          modifyMS $ \(v,_) -> (v, s)

      where file = "State/vixen"

------------------------------------------------------------------------

vixen :: (String -> WTree) -> String -> IO String
vixen k key = P.unpack `fmap` random (k key)

random :: WTree -> IO P.ByteString
random (Leaf a)  = return a
random (Node ls) = randomElem ls >>= random

mkResponses :: RChoice -> String -> WTree
mkResponses choices them = (\((_,wtree):_) -> wtree) $
    filter (\(reg,_) -> matchRegex reg them) choices

------------------------------------------------------------------------
--
-- serialisation for the vixen state
--
-- The tree of regexes and responses is written in binary form to
-- State/vixen, and we suck it in on module init, then lazily regexify it all
--

data WTree = Leaf !P.ByteString | Node ![WTree]

instance Binary WTree where
    put_ h (Leaf s)  = do
        putByte h 0
        put_ h s
    put_ h (Node ls) = do
        putByte h 1
        put_ h ls
    get h = do
        tag <- getWord8 h
        case tag of
            0 -> Leaf `fmap` get h
            1 -> Node `fmap` get h

type Choice  = [(P.ByteString, WTree)]
type RChoice = [(Regex, WTree)] -- compiled choices

