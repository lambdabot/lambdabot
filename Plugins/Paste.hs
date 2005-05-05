--
-- | Persistent state
--
module Plugins.Paste (theModule) where

import Lambdabot
import Control.Concurrent
import Control.Monad.Trans (liftIO)

newtype PasteModule = PasteModule ()

theModule :: MODULE
theModule = MODULE $ PasteModule ()

announceTarget :: String
announceTarget = "#haskell"

instance Module PasteModule ThreadId where
    moduleHelp    _ _ = return ""
    moduleCmds      _ = return []
    moduleInit      _ = do
      tid <- lbIO (\conv -> 
        forkIO $ pasteListener $ conv . ircPrivmsg announceTarget)
      writeMS tid
    moduleExit      _ = liftIO . killThread =<< readMS
    process _ _ _ _ _ = return ()


-- | Implements a server that listens for pastes from a paste script.
--   Authentification is done via...
pasteListener :: (String -> IO ()) -> IO ()
pasteListener say = do
  -- ...
  say "someone has pasted something somewhere"
  -- ...
