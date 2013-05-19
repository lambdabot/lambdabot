--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- Syntactic sugar for developing plugins.
-- Simplifies import lists, and abstracts over common patterns
--
module Lambdabot.Plugin
    ( Module(..)
    , ModuleT
    , newModule
    
    , getModuleName
    , bindModule0
    , bindModule1
    , bindModule2
    
    , LB
    , MonadLB(..)
    , lim80
    , ios80
    
    , ChanName
    , mkCN
    , getCN
    
    , Nick(..)
    , ircPrivmsg
    
    , module Lambdabot.Config
    , module Lambdabot.Config.Core
    , module Lambdabot.Command
    , module Lambdabot.State
    , module Lambdabot.File
    , module Lambdabot.Util
    , module Lambdabot.Util.Serial
    ) where

import Lambdabot
import Lambdabot.ChanName
import Lambdabot.Config
import Lambdabot.Config.Core
import Lambdabot.Command hiding (runCommand, execCmd)
import Lambdabot.File
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.Nick
import Lambdabot.State
import Lambdabot.Util
import Lambdabot.Util.Serial

import Codec.Binary.UTF8.String
import Control.Monad
import Control.Monad.Trans
import Data.Char

lim80 :: Monad m => m String -> Cmd m ()
lim80 action = do
    to <- getTarget
    let lim = case nName to of
                  ('#':_) -> limitStr 80 -- message to channel: be nice
                  _       -> id          -- private message: get everything
        spaceOut = unlines . map (' ':) . lines
        removeControl = filter (\x -> isSpace x || not (isControl x))
    (say =<<) . lift $ liftM (lim . encodeString . spaceOut . removeControl . decodeString) action 

-- | convenience, similar to ios but also cut output to channel to 80 characters
-- usage:  @process _ _ to _ s = ios80 to (plugs s)@
ios80 :: MonadIO m => IO String -> Cmd m ()
ios80 = lim80 . io
