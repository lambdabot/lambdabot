module Lambdabot.Main
    ( main'
    , defaultConfig
    , received
    ) where

import Lambdabot
import Lambdabot.Config
import Lambdabot.Shared

import Control.Applicative
import Control.Monad.State (get, liftIO)
import qualified Data.Map as M
import System.Environment

parseArgs :: [String] -> Maybe [String]
parseArgs ("-e" : cmd : x)  = (cmd :) <$> parseArgs x
parseArgs []                = Just []
parseArgs _                 = Nothing

main' :: Config -> Maybe DynLoad -> (LB (), [String]) -> IO ()
main' config dyn (loadStaticModules, pl) = do
    args <- parseArgs <$> getArgs
    
    let ld = maybe (error "no dynamic loading") id dyn
    case args of
        Just xs -> runIrc config (if null xs then ["offline"] else xs) loadStaticModules ld pl
        _       -> putStrLn "Usage: lambdabot [-e 'cmd']*"


------------------------------------------------------------------------

received :: IrcMessage -> LB ()
received msg = do s   <- get
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
