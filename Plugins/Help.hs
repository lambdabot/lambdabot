--
-- | Provide help for plugins
--
module Plugins.Help (theModule) where

import Lambdabot
import Util                 (showClean)
import Control.Exception    (Exception(..), evaluate)
import Control.Monad.Error  (catchError, throwError)
import Control.Monad.Trans

newtype HelpModule = HelpModule ()

theModule :: MODULE
theModule = MODULE $ HelpModule ()

instance Module HelpModule () where
    moduleHelp _ _ = "@help <command> - ask for help for <command>" -- default output
    moduleCmds   _ = ["help"]
    process _ _ target cmd rest = doHelp target cmd rest

doHelp :: String -> String -> [Char] -> ModuleT () LB ()

doHelp target cmd "" = doHelp target cmd "help"

--
-- If a target is a command, find the associated help, otherwise if it's
-- a module, return a list of commands that module implements.
--
doHelp target cmd rest = 
    withModule ircCommands arg                  -- see if it is a command
        (withModule ircModules arg              -- else maybe it's a module name
            (doHelp target cmd "help")          -- else give up
            -- its a module
            (\md -> do
                let ss = moduleCmds md
                let s | null ss   = arg ++ " is a module."
                      | otherwise = arg ++ " provides: " ++ showClean ss
                ircPrivmsg target s))

        -- so it's a valid command, try to find its help
        -- could fail if strings are wrong.
        (\md -> do
            s <- catchError (liftIO $ evaluate $ moduleHelp md arg) $ \e ->
                case e of
                    IRCRaised (NoMethodError _) -> return "This command is unknown."
                    _                           -> throwError e
            ircPrivmsg target s)

    where (arg:_) = words rest
