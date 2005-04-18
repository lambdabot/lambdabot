--
-- | Dynamic module, binding to the dynamic linker
-- Reimplemented to use hs-plugins.
--
module Plugins.DynamicModule (theModule) where

import {-# SOURCE #-} Modules   (plugins)
import IRC

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
    moduleCmds   _ = return ["dynamic-load","dynamic-unload","dynamic-reload"]

    moduleInit   _ = do 
        liftIO $ putStr "Loading plugins\t" >> hFlush stdout
        mapM_ (\p -> load p >> liftIO (putChar '.' >> hFlush stdout)) plugins
        liftIO $ putStrLn " done."
                                
    process _ msg src "dynamic-load" rest =
        checkPrivs msg src $ load rest >> ircPrivmsg src "module loaded"

    process _ msg src "dynamic-unload" rest =
        checkPrivs msg src $ unload rest >> ircPrivmsg src "module unloaded"

    process _ msg src "dynamic-reload" rest = do
        checkPrivs msg src $ do
            unload rest ; load rest ; ircPrivmsg src "module reloaded"

    process _ _ _ _ _ = error "DynamicModule: Invalid command"

--
-- | Load value "theModule" from each plugin, given simple name of a
-- plugin, i.e. "google"
--
load :: (MonadLB m) => String -> m ()
load nm = do
        let file = getModuleFile nm
        catchError (do (_,md) <- liftLB $ ircLoad file "theModule"
                       liftLB $ ircInstallModule md nm) -- need to put mod in state
                   (\e -> (liftLB $ ircUnload file) >> throwError e)

--
-- | Unload a module, e.g. "vixen"
--
unload :: (MonadLB m) => [Char] -> m ()
unload nm = do
        unless (nm `elem` plugins) $ error "unknown or static module"
        liftLB $ ircUnloadModule nm
        liftLB $ ircUnload (getModuleFile nm)

--
-- | Convert simple plugin name to a valid plugin. Could sanity check.
--
getModuleFile :: [Char] -> [Char]
getModuleFile s = "Plugins/" ++ upperise s ++ "Module.o"
    where
        upperise :: [Char] -> [Char]
        upperise []     = []
        upperise (c:cs) = toUpper c:cs
