--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- Syntactic sugar for developing plugins.
-- Simplifies import lists, and abstracts over common patterns
--
module Plugin (
        ios, box, list, ios80,

        module Lambdabot,
        module LBState,
        module Config,

        module Lambdabot.Util,
        module Lambdabot.Serial,
        module Lambdabot.Process,
        module Lambdabot.MiniHTTP,
        module Lambdabot.Url,
        module Lambdabot.Regex,

        module Data.List,
        module Data.Char,
        module Data.Maybe,
        module Data.Either,
        module System.IO,

        module Control.Monad.Error

    ) where

import Lambdabot
import LBState
import Config

import Lambdabot.Util
import Lambdabot.Serial
import Lambdabot.Process
import Lambdabot.MiniHTTP
import Lambdabot.Url
import Lambdabot.Regex

import Message

import Data.List
import Data.Char
import Data.Maybe
import Data.Either

import System.IO

import Control.Monad.Error
import Control.Monad.Trans

-- | convenience, we often want to perform some io, get a string, and box it.
ios  :: (Functor m, MonadIO m) => IO a -> m [a]
ios  = list . io

list :: (Functor m, Monad m) => m a -> m [a]
list = (return `fmap`)

box :: (Functor m, Monad m) => a -> m [a]
box = return . return

-- | convenience, similar to ios but also cut output to channel to 80 characters
-- usage:  @process _ _ to _ s = ios80 to (plugs s)@
ios80 :: (Functor m, MonadIO m) => Nick -> IO String -> m [String]
ios80 to what = list . io $ what >>= return . lim
    where lim = case nName to of
                    ('#':_) -> abbr 80 -- message to channel: be nice
                    _       -> id      -- private message: get everything
          abbr n s = let (b, t) = splitAt n s in
                     if null t then b else take (n-3) b ++ "..."
