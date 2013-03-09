{-# LANGUAGE DeriveDataTypeable #-}
module Lambdabot.Error where

import Control.Exception
import Data.Typeable
import Lambdabot.Util.Signals

-- A type for handling both Haskell exceptions and external signals
data IRCError 
    = IRCRaised SomeException
    | SignalCaught Signal
    deriving Typeable

instance Exception IRCError

instance Show IRCError where
    show (IRCRaised (SomeException e)) = show (typeOf e) ++ ": "   ++ show e
    show (SignalCaught s)              = "Signal: " ++ ircSignalMessage s

