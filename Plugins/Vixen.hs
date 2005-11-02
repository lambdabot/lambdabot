--
-- | Talk to hot chixxors.
--
-- (c) Mark Wotton
--
module Plugins.Vixen where

import Lambdabot
import LBState
import Plugins.Vixen.VixenState

import Util (stdGetRandItem)

import Data.Maybe (isJust)

import qualified Data.FastPackedString as P
import Text.Regex

import Control.Monad.State      (MonadIO, liftIO)

------------------------------------------------------------------------

newtype VixenModule = VixenModule ()

theModule :: MODULE
theModule = MODULE $ VixenModule ()

instance Module VixenModule (String -> IO String) where
    moduleSticky _ = False

    moduleHelp _ s = return $ case s of
             "vixenlove" -> "talk to me, big boy"
             _           -> "sergeant curry's lonely hearts club"

    moduleDefState _ = return $ mkVixen

    moduleCmds     _ = return ["vixen"]
    process _ _ src cmd rest = case cmd of
               "vixen" -> vixenCmd src rest
               _       -> error "vixen error: i'm just a girl!"

vixenCmd :: String -> String -> ModuleT (String -> IO String) LB ()
vixenCmd src rest = do 
	responder <-  readMS
        result <- liftIO $  responder rest
        ircPrivmsg src result


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
match r s = isJust $ matchRegex r s

mkResponses :: RespChoice -> String -> WTree
mkResponses choices them = 
    (\((_,wtree):_) -> wtree) $ filter (\(reg,_) -> match reg them) choices

