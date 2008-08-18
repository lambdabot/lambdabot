{-# LANGUAGE MultiParamTypeClasses #-}

-- | Skeletal paste support
module Plugin.Paste (theModule) where

import Plugin
import Control.Concurrent
import Message

PLUGIN Paste

announceTarget = "#haskell" -- hmm :/

instance Module PasteModule ThreadId where
    moduleInit _ = do
      tid <- lbIO (\conv ->
        forkIO $ pasteListener $ conv . ircPrivmsg announceTarget . Just)
      writeMS tid
    moduleExit _ = io . killThread =<< readMS


-- | Implements a server that listens for pastes from a paste script.
--   Authentification is done via...
pasteListener :: (String -> IO ()) -> IO ()
pasteListener say = do
  -- ...
  say "someone has pasted something somewhere"
  -- ...
