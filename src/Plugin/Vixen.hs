{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

-- | Talk to hot chixxors.

-- (c) Mark Wotton
-- Serialisation (c) 2007 Don Stewart

module Plugin.Vixen where

import Data.Binary

import Control.Arrow ((***))
import System.Directory
import qualified Data.ByteString.Char8 as P

import Lambdabot.File (findFile)
import Plugin

plugin "Vixen"

instance Module VixenModule where
    type ModuleState VixenModule = (Bool, String -> IO String)
    
    moduleCmds _ = 
        [ (command "vixen")
            { help = say "vixen <phrase>. Sergeant Curry's lonely hearts club"
            , process = \txt -> lift readMS >>= (ios . ($ txt) . snd) >>= mapM_ say
            }
        , (command "vixen-on")
            { privileged = True
            , help = do
                me <- showNick =<< getLambdabotName
                say ("vixen-on: turn " ++ me ++ " into a chatterbot")
            , process = const $ do
                lift $ modifyMS $ \(_,r) -> (True, r)
                say "What's this channel about?"
            }
        , (command "vixen-off")
            { privileged = True
            , help = do
                me <- showNick =<< getLambdabotName
                say ("vixen-off: shut " ++ me ++ "up")
            , process = const $ do
                lift $ modifyMS $ \(_,r) -> (False, r)
                say "Bye!"
            }
        ]
    
    -- if vixen-chat is on, we can just respond to anything
    contextual _ txt = do
        (alive, k) <- lift readMS
        if alive then ios (k txt) >>= mapM_ say
                 else return ()

    moduleDefState _ = return (False, const (return "<undefined>"))

    -- suck in our (read only) regex state from disk
    -- compile it, and stick it in the plugin state
    moduleInit _     = do
      b <- io $ doesFileExist =<< findFile "vixen"
      when b $ do
          s <- io $ do st <- decodeFile =<< findFile "vixen"
                       let compiled = map (regex *** id) st
                       return (vixen (mkResponses compiled))
          modifyMS $ \(v,_) -> (v, s)

------------------------------------------------------------------------

vixen :: (String -> WTree) -> String -> IO String
vixen k key = P.unpack `fmap` randomW (k key)

randomW :: WTree -> IO P.ByteString
randomW (Leaf a)  = return a
randomW (Node ls) = randomElem ls >>= randomW

mkResponses :: RChoice -> String -> WTree
mkResponses choices them = (\((_,wtree):_) -> wtree) $
    filter (\(reg,_) -> matches' reg them) choices

------------------------------------------------------------------------
-- serialisation for the vixen state
--
-- The tree of regexes and responses is written in binary form to
-- State/vixen, and we suck it in on module init, then lazily regexify it all

data WTree = Leaf !P.ByteString | Node ![WTree]
             deriving Show

instance Binary WTree where
    put (Leaf s)  = putWord8 0 >> put s
    put (Node ls) = putWord8 1 >> put ls
    get = do
        tag <- getWord8
        case tag of
            0 -> liftM Leaf get
            1 -> liftM Node get

type Choice  = [(P.ByteString, WTree)]
type RChoice = [(Regex, WTree)] -- compiled choices

{- Sample of how to rescue data in the old 32-bit Binary format

newtype OldChoice = OC { unOC :: Choice }
                    deriving Show
instance Binary OldChoice where
    get = do liftM OC (getList (getPair getBS getWTree))
        where
          getList :: (Get a) -> Get [a]
          getList getA = do
            n <- liftM fromIntegral (get :: Get Int32)
            replicateM n getA

          getShort :: Get Int
          getShort = liftM fromIntegral (get :: Get Int32)

          getPair = liftM2 (,)

          getBS   = getShort >>= getByteString

          getWTree = do
            tag <- getWord8
            case tag of
              0 -> liftM Leaf getBS
              1 -> liftM Node (getList getWTree)
-}
