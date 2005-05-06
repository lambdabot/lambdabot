--
-- | Dynamic module, binding to the dynamic linker
-- Reimplemented to use hs-plugins.
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
    moduleHelp _ _ = return 
        "@dynamic-[load,unload,reload] <module>, interface to dynamic linker"
    moduleCmds   _ = return []
    modulePrivs  _ = return ["dynamic-load","dynamic-unload","dynamic-reload"]


    moduleInit   _ = do 
        liftIO $ putStr "Loading plugins\t" >> hFlush stdout
        mapM_ (\p -> load p >> liftIO (putChar '.' >> hFlush stdout)) plugins
        liftIO $ putStrLn " done."
                                
    process _ _ src "dynamic-load" rest =
        load rest >> ircPrivmsg src "module loaded"

    process _ _ src "dynamic-unload" rest =
        unload rest >> ircPrivmsg src "module unloaded"

    process _ _ src "dynamic-reload" rest =
        do unload rest ; load rest ; ircPrivmsg src "module reloaded"

    process _ _ _ _ _ = error "DynamicModule: Invalid command"


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
unload :: [Char] -> LB ()
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
