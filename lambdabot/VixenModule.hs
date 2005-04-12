--
-- | Talk to hot chixxors.
-- (c) Mark Wotton
--

module VixenModule where

import IRC
import Vixen			(mkResponses,readConfig,vixen,RespChoice)

import Control.Monad.State      (MonadIO, liftIO)

------------------------------------------------------------------------

newtype VixenModule = VixenModule ()

theModule :: MODULE
theModule = MODULE vixenModule

vixenModule :: VixenModule
vixenModule = VixenModule ()

file :: String
file = "data/vixenrc"

instance Module VixenModule RespChoice where
    moduleName   _ = "vixen"
    moduleSticky _ = False

    moduleHelp _ s = return $ case s of
             "vixenlove" -> "talk to me, big boy"
             _           -> "sergeant curry's lonely hearts club"

    moduleDefState _ = liftIO (readConfig file)
    
    moduleCmds     _ = return ["vixen"]
    process _ _ src cmd rest = case cmd of
               "vixen" -> vixenCmd src rest
               _       -> error "vixen error: i'm just a girl!"

--
-- ideally, mkResponses state would be cached between calls - the file could be large.
--
vixenCmd :: String -> String -> ModuleT RespChoice IRC ()
vixenCmd src rest = do 
	state <-  readMS
        result <- liftIO $ vixen (mkResponses state) rest
        ircPrivmsg src result

