{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
-- | Provide help for plugins
module Plugin.Help (theModule) where

import Plugin
import Lambdabot

plugin "Help"

instance Module HelpModule where
    moduleCmds = return
        [ (command "help")
            { help = say "help <command>. Ask for help for <command>. Try 'list' for all commands"
            , process = \args -> withMsg $ \msg -> do
                tgt <- getTarget
                lb (doHelp msg tgt args) >>= mapM_ say
            }
        ]

moduleHelp theCmd msg tgt cmd =
    execCmd (help theCmd) msg tgt cmd

--
-- If a target is a command, find the associated help, otherwise if it's
-- a module, return a list of commands that module implements.
--
doHelp msg tgt [] = doHelp msg tgt "help"
doHelp msg tgt rest =
    withCommand arg                  -- see if it is a command
        (withModule arg              -- else maybe it's a module name
            (doHelp msg tgt "help")             -- else give up
            (\md -> do -- its a module
                cmds <- moduleCmds
                let ss = cmds >>= cmdNames
                let s | null ss   = arg ++ " is a module."
                      | otherwise = arg ++ " provides: " ++ showClean ss
                return [s]))

        -- so it's a valid command, try to find its help
        (\md theCmd -> moduleHelp theCmd msg tgt arg)

    where (arg:_) = words rest
