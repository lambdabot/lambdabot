{-# OPTIONS -fglasgow-exts #-}
module HelpModule where

import IRC
import qualified Map as M
import Control.Monad.State ( gets )
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader

newtype HelpModule = HelpModule ()

theModule :: MODULE
theModule = MODULE helpModule

helpModule :: HelpModule
helpModule = HelpModule ()

instance Module HelpModule () where
    moduleName   _ = return "help"
    moduleHelp   _ _ = return " @help <command> - ask for help for <command>" -- default output
    moduleSticky _ = False
    commands     _ = return ["help"]
    process        = doHelp

doHelp :: HelpModule -> IRCMessage -> String -> String -> [Char] -> TrivIRC ()

doHelp m msg target cmd "" = doHelp m msg target cmd "help"

doHelp m msg target cmd rest = do
    mmod <- gets (M.lookup arg . ircCommands)
    case mmod of
        Nothing -> process m msg target cmd "help" -- then default msg
        Just (ModuleRef md ref) -> do { 
                         -- this nonsense is to avoid evaluating moduleHelp too early
    ;helpString <-
        catchError (return () >> liftLB (moduleHelp md arg `runReaderT` ref)) $ \e ->
        case e of        -- module doesn't define moduleHelp
            IRCRaised (NoMethodError _) -> return "no help for this command"
            _ -> throwError e

    ;ircPrivmsg target helpString
    }

    where (arg:_) = words rest

