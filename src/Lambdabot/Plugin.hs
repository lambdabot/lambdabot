{-# LANGUAGE TemplateHaskell #-}

--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- Syntactic sugar for developing plugins.
-- Simplifies import lists, and abstracts over common patterns
--
module Lambdabot.Plugin (
        Module(..), ModuleT, newModule, modules,
        
        getModuleName,
        bindModule0, bindModule1, bindModule2, 
        
        LB, lb, ios, ios80,
        
        Nick(..), packNick, unpackNick, ircPrivmsg,
        
        module Lambdabot.Config,
        module Lambdabot.Command,
        module Lambdabot.State,
        
        module Lambdabot.File,
        module Lambdabot.Util.MiniHTTP,
        module Lambdabot.Util.Process,
        module Lambdabot.Util.Regex,
        module Lambdabot.Util.Serial,
        module Lambdabot.Util.Url,
        module Lambdabot.Util,
        
        module Data.Char,
        module Data.Either,
        module Data.List,
        module Data.Maybe,
        module System.FilePath,
        module System.IO,
        
        module Control.Monad.Error
    ) where

import Lambdabot.Config
import Lambdabot
import Lambdabot.Module
import Lambdabot.Command hiding (runCommand, execCmd)
import Lambdabot.State

import Lambdabot.File (findLBFile)
import Lambdabot.Message
import Lambdabot.Util.MiniHTTP
import Lambdabot.Util.Process
import Lambdabot.Util.Regex
import Lambdabot.Util.Serial
import Lambdabot.Util.Url
import Lambdabot.Util

import Data.List
import Data.Char
import Data.Maybe
import Data.Either

import System.IO
import System.FilePath ((</>), (<.>))

import Control.Monad.Error

import Codec.Binary.UTF8.String

import Language.Haskell.TH

-- | convenience, we often want to perform some io, get a string, and say it.
ios  :: (MonadIO m) => IO String -> Cmd m ()
ios x = io x >>= say

-- | convenience, similar to ios but also cut output to channel to 80 characters
-- usage:  @process _ _ to _ s = ios80 to (plugs s)@
ios80 :: (Functor m, MonadIO m) => IO String -> Cmd m ()
ios80 action = do
    to <- getTarget
    let lim = case nName to of
                  ('#':_) -> limitStr 80 -- message to channel: be nice
                  _       -> id          -- private message: get everything
        spaceOut = unlines . map (' ':) . lines
        removeControl = filter (\x -> isSpace x || not (isControl x))
    (say =<<) . io $ fmap (lim . encodeString . spaceOut . removeControl . decodeString) action 

modules :: [String] -> Q Exp
modules xs = [| ($install, $names) |]
 where
    names = listE $ map (stringE . map toLower) xs
    install = [| sequence_ $(listE $ map instalify xs) |]
    instalify x = let mod = varE $ mkName $ concat $ ["Lambdabot.Plugin.", x, ".theModule"]
                      low = stringE $ map toLower x
                  in [| ircInstallModule (MODULE $mod) $low |]
