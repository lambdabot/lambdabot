{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
-- | Provide help for plugins
module Plugin.Help (theModule) where

import Plugin
import Control.Exception (NoMethodError(..), fromException)
import qualified Lambdabot.Command as Cmd

plugin "Help"

instance Module HelpModule where
    moduleCmds   _ =
        [ (command "help")
            { help = say "help <command>. Ask for help for <command>. Try 'list' for all commands"
            , process = \args -> withMsg $ \msg -> do
                tgt <- getTarget
                lb (doHelp msg tgt args) >>= mapM_ say
            }
        ]

moduleHelp m cmd msg tgt = fmap unlines (Cmd.execCmd (help theCmd) msg tgt cmd)
    where Just theCmd = lookupCmd m cmd

--
-- If a target is a command, find the associated help, otherwise if it's
-- a module, return a list of commands that module implements.
--
doHelp msg tgt [] = doHelp msg tgt "help"
doHelp msg tgt rest =
    withModule ircCommands arg                  -- see if it is a command
        (withModule ircModules arg              -- else maybe it's a module name
            (doHelp msg tgt "help")             -- else give up
            (\md -> do -- its a module
                let ss = moduleCmds md >>= Cmd.cmdNames
                let s | null ss   = arg ++ " is a module."
                      | otherwise = arg ++ " provides: " ++ showClean ss
                return [s]))

        -- so it's a valid command, try to find its help
        (\md -> do
            s <- catchError (moduleHelp md arg msg tgt) $ \e ->
                case e of
                    IRCRaised (fromException -> Just (NoMethodError _)) 
                        -> return "This command is unknown."
                    _   -> throwError e
            return [s])

    where (arg:_) = words rest
