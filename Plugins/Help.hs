--
-- | Provide help for plugins
--
module Plugins.Help (theModule) where

import IRC
import Util                 (showClean)
import Control.Exception    (Exception(..))
import Control.Monad.Error  (catchError, throwError)

newtype HelpModule = HelpModule ()

theModule :: MODULE
theModule = MODULE $ HelpModule ()

instance Module HelpModule () where
    moduleHelp _ _ = return "@help <command> - ask for help for <command>" -- default output
    moduleCmds   _ = return ["help"]
    process        = doHelp

doHelp :: HelpModule -> IRCMessage -> String -> String -> [Char] -> ModuleT () IRC ()

doHelp m msg target cmd "" = doHelp m msg target cmd "help"

--
-- If a target is a command, find the associated help, otherwise if it's
-- a module, return a list of commands that module implements.
--
doHelp m msg target cmd rest = 
    withModule ircCommands arg                  -- see if it is a command
        (withModule ircModules arg              -- else maybe it's a module name
            (process m msg target cmd "help")   -- else give up
            -- its a module
            (\md -> do
                ss <- moduleCmds md
                let s | null ss   = arg ++ " is a module."
                      | otherwise = arg ++ " provides: " ++ (showClean ss)
                ircPrivmsg target s))

        -- so it's a valid command, try to find its help
        (\md -> do
            s <- catchError (moduleHelp md arg) $ \e ->
                case e of
                    IRCRaised (NoMethodError _) -> return "This command is unknown."
                    _                           -> throwError e
            ircPrivmsg target s)

    where (arg:_) = words rest
