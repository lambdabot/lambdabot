module HelpModule (theModule) where

import IRC
import Control.Exception
import Control.Monad.Error
import Control.Monad.Reader

newtype HelpModule = HelpModule ()

theModule :: MODULE
theModule = MODULE $ HelpModule ()

instance Module HelpModule () where
    moduleHelp _ _ = return " @help <command> - ask for help for <command>" -- default output
    moduleCmds   _ = return ["help"]
    process        = doHelp

doHelp :: HelpModule -> IRCMessage -> String -> String -> [Char] -> ModuleT () IRC ()

doHelp m msg target cmd "" = doHelp m msg target cmd "help"

doHelp m msg target cmd rest = withModule ircCommands arg
    (process m msg target cmd "help") {- then default msg -} (\md -> do
      helpString <- catchError (liftLB $ moduleHelp md arg) $ \e ->
        case e of        -- module doesn't define moduleHelp
            IRCRaised (NoMethodError _) -> return "no help for this command"
            _ -> throwError e
      ircPrivmsg target helpString)
  where (arg:_) = words rest
