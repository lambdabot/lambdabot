{-# LANGUAGE TemplateHaskell #-}
module Lambdabot.Main
    ( onStartupCmds
    , lambdabotMain
    , runIrc
    
    , DSum(..)
    , received
    
    , Modules
    , modules
    ) where

import Lambdabot
import Lambdabot.Config
import Lambdabot.Config.Core
import Lambdabot.IRC
import Lambdabot.Monad

import Control.Applicative
import Control.Monad.State (get, liftIO)
import Data.Char
import Data.Dependent.Sum
import qualified Data.Map as M
import Language.Haskell.TH
import System.Environment

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
    handleIrc (liftIO . putStrLn . ("Main: caught (and ignoring) "++). show) (f msg)
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
