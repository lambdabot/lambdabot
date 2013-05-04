{-# LANGUAGE TemplateHaskell #-}
module Lambdabot.Main
    ( DSum(..)
    , lambdabotMain
    , runIrc

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

import Control.Applicative
import Control.Exception.Lifted as E
import Data.Char
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import Data.Typeable
import Language.Haskell.TH
import System.Environment
import System.Exit
import System.Log.Formatter
import qualified System.Log.Logger as L
import System.Log.Handler.Simple
import Network (withSocketsDo)

parseArgs :: [String] -> Maybe [String]
parseArgs ("-e" : cmd : x)  = (cmd :) <$> parseArgs x
parseArgs []                = Just []
parseArgs _                 = Nothing

lambdabotMain :: Modules -> [DSum Config] -> IO ()
lambdabotMain loadStaticModules configuration = do
    args <- parseArgs <$> getArgs
    
    case args of
        Just xs -> runIrc loadStaticModules ((onStartupCmds :=> if null xs then ["offline"] else xs) : configuration)
        _       -> putStrLn "Usage: lambdabot [-e 'cmd']*"

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
runIrc :: LB () -> [DSum Config] -> IO a
runIrc initialise configBindings = withSocketsDo $ do
    rost <- initRoState (D.fromList configBindings)
    r <- try $ evalLB (do setupLogging
                          noticeM "Initialising plugins"
                          initialise
                          noticeM "Done loading plugins"
                          withIrcSignalCatch (reportInitDone rost >> mainLoop))
                       rost initRwState

    -- clean up and go home
    case r of
        Left (SomeException er) -> do
            case cast er of
                Just code -> exitWith code
                Nothing -> do
                    putStrLn "exception:"
                    print er
                    exitWith (ExitFailure 1)
        Right _ -> do
            exitWith ExitSuccess

-- Actually, this isn't a loop anymore.  TODO: better name.
mainLoop :: LB ()
mainLoop = do
    waitForQuit `E.catch`
        (\e@SomeException{} -> errorM (show e)) -- catch anything, print informative message, and clean up
    
    withAllModules moduleExit
    flushModuleState

    -- this kills profiling output:
    io $ exitWith (ExitFailure 1)

------------------------------------------------------------------------

type Modules = LB ()

modules :: [String] -> Q Exp
modules xs = [| sequence_ $(listE $ map instalify xs) |]
    where
        instalify x =
            let module' = varE $ mkName $ concat $ ["Lambdabot.Plugin.", x, ".theModule"]
                low     = stringE $ map toLower x
             in [| ircLoadModule $module' $low |]
