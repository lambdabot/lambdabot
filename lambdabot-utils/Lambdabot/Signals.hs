{-# LANGUAGE CPP, DeriveDataTypeable #-}

-- | The signal story.
-- Posix signals are external events that invoke signal handlers in
-- Haskell. The signal handlers in turn throw dynamic exceptions.  Our
-- instance of MonadError for LB maps the dynamic exceptions to
-- SignalCaughts, which can then be caught by a normal catchIrc or
-- handleIrc

-- Here's where we do that.
module Lambdabot.Signals where

#ifdef mingw32_HOST_OS
import Data.Typeable
import Control.Monad.Error

type Signal = String
newtype SignalException = SignalException Signal deriving Typeable

ircSignalMessage :: Signal -> [Char]
ircSignalMessage s = s

withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m = m

#else
import Lambdabot.Error
import Lambdabot.Util

import Data.Typeable

import Control.Concurrent (myThreadId, newEmptyMVar, putMVar, MVar, ThreadId)
import Control.OldException (throwDynTo)
import Control.Monad.Error

import System.IO.Unsafe
import System.Posix.Signals

-- A new type for the SignalException, must be Typeable so we can make a
-- dynamic exception out of it.
newtype SignalException = SignalException Signal deriving Typeable

--
-- A bit of sugar for installing a new handler
--
withHandler :: (MonadIO m,MonadError e m) => Signal -> Handler -> m () -> m ()
withHandler s h m
  = bracketError (io (installHandler s h Nothing))
                 (io . flip (installHandler s) Nothing)
                 (const m)

-- And more sugar for installing a list of handlers
withHandlerList :: (MonadError e m,MonadIO m)
                => [Signal] -> (Signal -> Handler) -> m () -> m ()
withHandlerList sl h m = foldr (withHandler `ap` h) m sl

--
-- Signals we care about. They're all fatal.
--
-- Be careful adding signals, some signals can't be caught and
-- installHandler just raises an exception if you try
--
ircSignalsToCatch :: [Signal]
ircSignalsToCatch = [
    busError,
    segmentationViolation,
    keyboardSignal,
    softwareTermination,
    keyboardTermination,
    lostConnection,
    internalAbort
    ]

--
-- User friendly names for the signals that we can catch
--
ircSignalMessage :: Signal -> [Char]
ircSignalMessage s
   | s==busError              = "SIGBUS"
   | s==segmentationViolation = "SIGSEGV"
   | s==keyboardSignal        = "SIGINT"
   | s==softwareTermination   = "SIGTERM"
   | s==keyboardTermination   = "SIGQUIT"
   | s==lostConnection        = "SIGHUP"
   | s==internalAbort         = "SIGABRT"
-- this case shouldn't happen if the list of messages is kept up to date
-- with the list of signals caught
   | otherwise                  = "killed by unknown signal"

--
-- The actual signal handler. It is this function we register for each
-- signal flavour. On receiving a signal, the signal handler maps the
-- signal to a a dynamic exception, and throws it out to the main
-- thread. The LB MonadError instance can then do its trickery to catch
-- it in handler/catchIrc
--
ircSignalHandler :: ThreadId -> Signal -> Handler
ircSignalHandler threadid s
  = CatchOnce $ do
        putMVar catchLock ()
        releaseSignals
        throwDynTo threadid $ SignalException s

--
-- | Release all signal handlers
--
releaseSignals :: IO ()
releaseSignals =
    flip mapM_ ircSignalsToCatch
               (\sig -> installHandler sig Default Nothing)

--
-- Mututally exclusive signal handlers
--
-- This is clearly a hack, but I have no idea how to accomplish the same
-- thing correctly. The main problem is that signals are often thrown
-- multiple times, and the threads start killing each other if we allow
-- the SignalException to be thrown more than once.
{-# NOINLINE catchLock #-}
catchLock :: MVar ()
catchLock = unsafePerformIO newEmptyMVar

--
-- | Register signal handlers to catch external signals
--
withIrcSignalCatch :: (MonadError e m,MonadIO m) => m () -> m ()
withIrcSignalCatch m = do
    io $ installHandler sigPIPE Ignore Nothing
    io $ installHandler sigALRM Ignore Nothing
    threadid <- io myThreadId
    withHandlerList ircSignalsToCatch (ircSignalHandler threadid) m
#endif
