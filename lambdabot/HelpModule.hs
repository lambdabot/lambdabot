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
    moduleHelp   _ _ = return "@help <command>"
    moduleSticky _ = False
    commands     _ = return ["help"]
    process      m msg target cmd "" = process m msg target cmd "help"
    process      m msg target cmd rest = do
        mmod <- gets (M.lookup arg . ircCommands)
        case mmod of
            Nothing -> process m msg target cmd "help"
            Just (MODULE md) -> do
                                                -- this nonsense is to avoid evaluating moduleHelp too early
                helpString <- liftLB $ catchError (return () >>= \() -> moduleHelp md arg) $ \e ->
                                            case e of
                                                IRCRaised (NoMethodError s) -> return "no help"
                                                _                           -> throwError e
                ircPrivmsg target helpString
      where (arg:_) = words rest
