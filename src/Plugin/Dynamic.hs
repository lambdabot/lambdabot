{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 2004-06 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | Dynamic module, binding to the dynamic linker
--
module Plugin.Dynamic (theModule) where

import Plugin
import Control.Monad.State

$(plugin "Dynamic")

modHelp = "An interface to dynamic linker"

instance Module DynamicModule where

    moduleSticky _ = True
    moduleCmds _ =
        [ (command "dynamic-load")
            { privileged = True
            , help = say modHelp
            , process = \rest -> do
                lift (lift (load rest))
                say "module loaded"
            }
        , (command "dynamic-unload")
            { privileged = True
            , help = say modHelp
            , process = \rest -> do
                lift (lift (unload rest))
                say "module unloaded"
            }
        , (command "dynamic-reload")
            { privileged = True
            , help = say modHelp
            , process = \rest -> do
                lift (lift (unload rest >> load rest))
                say "module reloaded"
            }
        ]

    moduleInit   _ = lift $ do
        plugins <- gets ircPlugins
        mapM_ (\p -> load p >> liftIO (putChar '!' >> hFlush stdout)) plugins

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
        plugins <- gets ircPlugins
        unless (nm `elem` plugins) $ error "unknown or static module"
        ircUnloadModule nm
        ircUnload (getModuleFile nm)

--
-- | Convert simple plugin name to a valid plugin. Could sanity check.
--
getModuleFile :: [Char] -> [Char]
getModuleFile s = "Plugin/" ++ upperise s ++ ".o"
    where
        upperise :: [Char] -> [Char]
        upperise []     = []
        upperise (c:cs) = toUpper c:cs
