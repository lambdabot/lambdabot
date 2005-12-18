--
-- | Dynamic module, binding to the dynamic linker
-- Reimplemented 2004-5 by dons to use hs-plugins.
--
module Plugins.Dynamic (theModule) where

import {-# SOURCE #-} Modules   (plugins)
import Lambdabot

import Data.Char                (toUpper)
import Control.Monad.Error
import Control.Monad.State
import System.IO                (stdout, hFlush)

newtype DynamicModule = DynamicModule ()

theModule :: MODULE
theModule = MODULE $ DynamicModule ()

instance Module DynamicModule () where

    moduleSticky _ = True
    moduleHelp _ _ = "An interface to dynamic linker"
    modulePrivs  _ = ["dynamic-load","dynamic-unload","dynamic-reload"]

    moduleInit   _ = do 
        liftIO $ putStr "Loading plugins\t" >> hFlush stdout
        mapM_ (\p -> load p >> liftIO (putChar '.' >> hFlush stdout)) plugins
        liftIO $ putStrLn " done."

    process_ _ s rest = case s of
        "dynamic-load"      -> 
                do load rest; return ["module loaded"]
        "dynamic-unload"    -> 
                do unload rest; return ["module unloaded"]
        "dynamic-reload"    -> 
                do unload rest; load rest; return ["module reloaded"]

--
-- | Load value "theModule" from each plugin, given simple name of a
-- plugin, i.e. "google"
--
load :: String -> LB Bool
load nm = do
    let file = getModuleFile nm
    catchError
        (do (_,md) <- ircLoad file "theModule"
            ircInstallModule md nm
            return True) -- need to put mod in state
        (\_ -> do
            ircUnload file
            liftIO $ putStrLn $ "\nCouldn't load "++nm++", ignoring"
            return False)

--
-- | Unload a module, e.g. "vixen"
--
unload :: String -> LB ()
unload nm = do
        unless (nm `elem` plugins) $ error "unknown or static module"
        ircUnloadModule nm
        ircUnload (getModuleFile nm)

--
-- | Convert simple plugin name to a valid plugin. Could sanity check.
--
getModuleFile :: [Char] -> [Char]
getModuleFile s = "Plugins/" ++ upperise s ++ ".o"
    where
        upperise :: [Char] -> [Char]
        upperise []     = []
        upperise (c:cs) = toUpper c:cs
