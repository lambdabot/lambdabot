{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | Provide help for plugins
module Plugin.Help (theModule) where

import Plugin
import Control.OldException (Exception(..), evaluate)

$(plugin "Help")

instance Module HelpModule () where
    moduleHelp _ _ = "help <command>. Ask for help for <command>. Try 'list' for all commands"
    moduleCmds   _ = ["help"]
    process_ _ cmd rest = lift $ doHelp cmd rest

doHelp :: String -> [Char] -> LB [String]
doHelp cmd [] = doHelp cmd "help"

--
-- If a target is a command, find the associated help, otherwise if it's
-- a module, return a list of commands that module implements.
--
doHelp cmd rest =
    withModule ircCommands arg                  -- see if it is a command
        (withModule ircModules arg              -- else maybe it's a module name
            (doHelp cmd "help")                 -- else give up
            (\md -> do -- its a module
                let ss = moduleCmds md
                let s | null ss   = arg ++ " is a module."
                      | otherwise = arg ++ " provides: " ++ showClean ss
                return [s]))

        -- so it's a valid command, try to find its help
        (\md -> do
            s <- catchError (liftIO $ evaluate $ moduleHelp md arg) $ \e ->
                case e of
                    IRCRaised (NoMethodError _) -> return "This command is unknown."
                    _                           -> throwError e
            return [s])

    where (arg:_) = words rest
