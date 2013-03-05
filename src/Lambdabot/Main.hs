{-# LANGUAGE TemplateHaskell #-}
module Lambdabot.Main
    ( onStartupCmds
    
    , DSum(..)
    , lambdabotMain
    , runIrc
    
    , received
    
    , Modules
    , modules
    ) where

import Lambdabot
import Lambdabot.Config
import Lambdabot.Config.Core
import Lambdabot.IRC
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.Util
import Lambdabot.Util.Signals

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import qualified Data.Dependent.Map as D
import Data.Dependent.Sum
import qualified Data.Map as M
import Data.Typeable
import Language.Haskell.TH
import System.Environment
import System.Exit
import System.IO
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

-- | The Lambdabot entry point.
-- Initialise plugins, connect, and run the bot in the LB monad
--
-- Also, handle any fatal exceptions (such as non-recoverable signals),
-- (i.e. print a message and exit). Non-fatal exceptions should be dealt
-- with in the mainLoop or further down.
runIrc :: LB a -> [DSum Config] -> IO ()
runIrc initialise configBindings = withSocketsDo $ do
    rost <- initRoState (D.fromList configBindings)
    r <- try $ evalLB (do withDebug "Initialising plugins" initialise
                          withIrcSignalCatch mainLoop)
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

-- Actually, this isn't a loop anymore.  FIXME: better name.
mainLoop :: LB ()
mainLoop = do
    catchIrc
        (do asks ircInitDoneMVar >>= io . flip putMVar ()
            asks ircQuitMVar >>= io . takeMVar)
        (\e -> do -- catch anything, print informative message, and clean up
            io $ hPutStrLn stderr $ case e of
                IRCRaised ex   -> "Exception: " ++ show ex
                SignalCaught s -> "Signal: " ++ ircSignalMessage s)
    
    withAllModules moduleExit
    flushModuleState
    
    -- this kills profiling output:
    io $ exitWith (ExitFailure 1)

-- | Print a debug message, and perform an action
withDebug :: String -> LB a -> LB ()
withDebug s a = do
    io $ hPutStr stderr (s ++ " ...")  >> hFlush stderr
    _ <- a
    io $ hPutStrLn stderr " done." >> hFlush stderr

------------------------------------------------------------------------

received :: IrcMessage -> LB ()
received msg = do
    s   <- get
    case M.lookup (ircMsgCommand msg) (ircCallbacks s) of
        Just cbs -> allCallbacks (map snd cbs) msg
        _        -> return ()

-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: [a -> LB ()] -> a -> LB ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg = do
    handleIrc (liftIO . putStrLn . ("Main: caught (and ignoring) "++) . show) (f msg)
    allCallbacks fs msg

------------------------------------------------------------------------

type Modules = LB ()

modules :: [String] -> Q Exp
modules xs = [| sequence_ $(listE $ map instalify xs) |]
    where
        instalify x = 
            let module' = varE $ mkName $ concat $ ["Lambdabot.Plugin.", x, ".theModule"]
                low     = stringE $ map toLower x
             in [| ircInstallModule $module' $low |]
