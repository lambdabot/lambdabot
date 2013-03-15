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

-- avoid wrapping multiple layers of (SomeException . IRCRaised)
instance Exception IRCError where
    toException (IRCRaised e)   = e
    toException other           = SomeException other
    
    fromException s@(SomeException e) = Just (maybe (IRCRaised s) id (cast e))

instance Show IRCError where
    show (IRCRaised (SomeException e)) = show (typeOf e) ++ ": "   ++ show e
    show (SignalCaught s)              = "Signal: " ++ ircSignalMessage s

