module HelpModule where

import IRC
import qualified Map as M
import Control.Monad.State ( gets )
import Control.Exception
import Control.Monad.Error

newtype HelpModule = HelpModule ()

theModule :: MODULE
theModule = MODULE helpModule

helpModule :: HelpModule
helpModule = HelpModule ()

instance Module HelpModule where
    moduleName   _ = return "help"
    moduleHelp   _ _ = return "@help <command>" -- default output
    moduleSticky _ = False
    commands     _ = return ["help"]
    process        = doHelp

doHelp :: (Module m) => m -> IRCMessage -> String -> String -> [Char] -> IRC ()

doHelp m msg target cmd "" = doHelp m msg target cmd "help"

doHelp m msg target cmd rest = do
    mmod <- gets (M.lookup arg . ircCommands)
    case mmod of
        Nothing -> process m msg target cmd "help" -- then default msg
        Just (MODULE md) -> do { 
                         -- this nonsense is to avoid evaluating moduleHelp too early
    ;helpString <- liftLB $ 
        catchError (return () >>= \() -> moduleHelp md arg) $ \e ->
        case e of        -- module doesn't define moduleHelp
            IRCRaised (NoMethodError _) -> return "no help for this command"
            _ -> throwError e

    ;ircPrivmsg target helpString
    }

    where (arg:_) = words rest

