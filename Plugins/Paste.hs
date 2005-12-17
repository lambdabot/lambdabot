--
-- | Skeletal paste support
--
module Plugins.Paste (theModule) where

import Lambdabot
import LBState
import Control.Concurrent
import Control.Monad.Trans (liftIO)

newtype PasteModule = PasteModule ()

theModule :: MODULE
theModule = MODULE $ PasteModule ()

announceTarget :: String
announceTarget = "#haskell" -- hmm :/

instance Module PasteModule ThreadId where
    moduleInit _ = do
      tid <- lbIO (\conv -> 
        forkIO $ pasteListener $ conv . ircPrivmsg announceTarget)
      writeMS tid
    moduleExit _ = liftIO . killThread =<< readMS


-- | Implements a server that listens for pastes from a paste script.
--   Authentification is done via...
pasteListener :: (String -> IO ()) -> IO ()
pasteListener say = do
  -- ...
  say "someone has pasted something somewhere"
  -- ...
