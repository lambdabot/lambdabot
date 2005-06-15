--
-- | Talk to hot chixxors.
--
-- (c) Mark Wotton
--
module Plugins.Vixen where

import Lambdabot
import LBState
import Plugins.Vixen.Vixen      (mkVixen)

import Control.Monad.State      (MonadIO, liftIO)

------------------------------------------------------------------------

newtype VixenModule = VixenModule ()

theModule :: MODULE
theModule = MODULE $ VixenModule ()

file :: String
file = "State/vixenrc"

instance Module VixenModule (String -> IO String) where
    moduleSticky _ = False

    moduleHelp _ s = return $ case s of
             "vixenlove" -> "talk to me, big boy"
             _           -> "sergeant curry's lonely hearts club"

    moduleDefState _ = do
                         f <- liftIO (readFile file)
    			 return $ mkVixen f

    moduleCmds     _ = return ["vixen"]
    process _ _ src cmd rest = case cmd of
               "vixen" -> vixenCmd src rest
               _       -> error "vixen error: i'm just a girl!"

vixenCmd :: String -> String -> ModuleT (String -> IO String) IRC ()
vixenCmd src rest = do 
	responder <-  readMS
        result <- liftIO $  responder rest
        ircPrivmsg src result

