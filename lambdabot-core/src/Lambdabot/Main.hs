{-# LANGUAGE TemplateHaskell #-}
module Lambdabot.Main
    ( lambdabotVersion
    
    , Config
    , DSum(..)
    , lambdabotMain

    , Modules
    , modules

    , module Lambdabot.Config.Core
    , Priority(..)
    ) where

import Lambdabot
import Lambdabot.Config
import Lambdabot.Config.Core
import Lambdabot.Logging
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.State
import Lambdabot.Util
import Lambdabot.Util.Signals

import Control.Exception.Lifted as E
import Data.Char
import Data.Dependent.Sum
import Data.Typeable
import Data.Version
import Language.Haskell.TH
import Paths_lambdabot
import System.Exit
import System.Log.Formatter
import qualified System.Log.Logger as L
import System.Log.Handler.Simple
import Network (withSocketsDo)

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
lambdabotMain :: LB () -> [DSum Config] -> IO ExitCode
lambdabotMain initialise cfg = withSocketsDo . withIrcSignalCatch $ do
    rost <- initRoState cfg
    r <- try $ evalLB (do setupLogging
                          noticeM "Initialising plugins"
                          initialise
                          noticeM "Done loading plugins"
                          reportInitDone rost
                          mainLoop
                          return ExitSuccess)
                       rost initRwState

    -- clean up and go home
    case r of
        Left (SomeException er) -> do
            case cast er of
                Just code -> return code
                Nothing -> do
                    putStrLn "exception:"
                    print er
                    return (ExitFailure 1)
        Right code -> return code

-- Actually, this isn't a loop anymore.  TODO: better name.
mainLoop :: LB ()
mainLoop = do
    waitForQuit `E.catch`
        (\e@SomeException{} -> errorM (show e)) -- catch anything, print informative message, and clean up
    
    withAllModules moduleExit
    flushModuleState


------------------------------------------------------------------------

type Modules = LB ()

modules :: [String] -> Q Exp
modules xs = [| sequence_ $(listE $ map instalify xs) |]
    where
        instalify x =
            let module' = varE $ mkName $ concat $ ["Lambdabot.Plugin.", x, ".theModule"]
                low     = stringE $ map toLower x
             in [| ircLoadModule $module' $low |]
