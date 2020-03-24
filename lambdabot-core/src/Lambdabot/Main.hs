{-# LANGUAGE TemplateHaskell #-}
module Lambdabot.Main
    ( lambdabotVersion
    
    , Config
    , DSum(..)
    , (==>)
    , lambdabotMain

    , Modules
    , modules

    , module Lambdabot.Plugin.Core
    , Priority(..)
    ) where

import Lambdabot.Bot
import Lambdabot.Config
import Lambdabot.Logging
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.Plugin.Core
import Lambdabot.Util
import Lambdabot.Util.Signals

import Control.Exception.Lifted as E
import Control.Monad.Identity
import Data.Dependent.Sum
import Data.List
import Data.IORef
import Data.Some
import Data.Version
import Language.Haskell.TH
import Paths_lambdabot_core (version)
import System.Exit
import System.Log.Formatter
import qualified System.Log.Logger as L
import System.Log.Handler.Simple
import Network.Socket (withSocketsDo)

lambdabotVersion :: Version
lambdabotVersion = version

setupLogging :: LB ()
setupLogging = do
    stream <- getConfig consoleLogHandle
    level  <- getConfig consoleLogLevel
    format <- getConfig consoleLogFormat
    
    unformattedHandler <- io (streamHandler stream level)
    let consoleHandler = unformattedHandler
            { formatter = simpleLogFormatter format }
    
    setRoot <- getConfig replaceRootLogger
    
    io $ if setRoot
        then L.updateGlobalLogger L.rootLoggerName
            (L.setLevel level . L.setHandlers [consoleHandler])
        else L.updateGlobalLogger "Lambdabot"
            (L.setLevel level . L.addHandler consoleHandler)

-- | The Lambdabot entry point.
-- Initialise plugins, connect, and run the bot in the LB monad
--
-- Also, handle any fatal exceptions (such as non-recoverable signals),
-- (i.e. print a message and exit). Non-fatal exceptions should be dealt
-- with in the mainLoop or further down.
lambdabotMain :: Modules -> [DSum Config Identity] -> IO ExitCode
lambdabotMain initialise cfg = withSocketsDo . withIrcSignalCatch $ do
    rost <- initRoState cfg
    rwst <- newIORef initRwState
    runLB (lambdabotRun initialise) (rost, rwst)
        `E.catch` \e -> do
            -- clean up and go home
            case fromException e of
                Just code -> return code
                Nothing   -> do
                    errorM (show e)
                    return (ExitFailure 1)

lambdabotRun :: Modules -> LB ExitCode
lambdabotRun ms = do
    setupLogging
    infoM "Initialising plugins"
    withModules ms $ do
        infoM "Done loading plugins"
        reportInitDone
        
        waitForQuit `E.catch`
            (\e@SomeException{} -> errorM (show e)) -- catch anything, print informative message, and clean up
    
    -- clean up any dynamically loaded modules
    mapM_ ircUnloadModule =<< listModules
    return ExitSuccess

------------------------------------------------------------------------

type Modules = [(String, Some Module)]

modules :: [String] -> Q Exp
modules xs = [| $(listE $ map instalify (nub xs)) |]
    where
        instalify x =
            let module' = varE $ mkName (x ++ "Plugin")
             in [| (x, Some $module') |]

withModules :: Modules -> LB a -> LB a
withModules []      = id
withModules ((n, Some m):ms)  = withModule n m . withModules ms

withModule :: String -> Module st -> LB a -> LB a
withModule name m = bracket_ (ircLoadModule name m) (ircUnloadModule name)
