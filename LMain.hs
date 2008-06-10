module LMain where

import Shared
import Lambdabot
import Message
import IRCBase

import Lambdabot.Util( listToMaybeAll )

import qualified Data.Map as M

import System.Environment

import Data.Maybe
import Control.Monad.State (get, liftIO)

data Args = Args { initcmds   :: [String] }

main' :: Maybe DynLoad -> (LB (), [String]) -> IO ()
main' dyn (loadStaticModules, pl) = do
    x    <- getArgs
    let a = parseArgs x
    case a of
        Just args -> runIrc (fromMaybe ["offline"] $ listToMaybeAll $ initcmds args) loadStaticModules ld pl
        _         -> putStrLn "Usage: lambdabot [-e 'cmd']*"

    where ld = fromMaybe (error "no dynamic loading") dyn
          parseArgs ("-e":cmd:x)       = parseArgs x >>$ \a -> a { initcmds = cmd : initcmds a }
          parseArgs []                 = Just $ Args { initcmds = [] }
          parseArgs _                  = Nothing
          (>>$) = flip fmap


------------------------------------------------------------------------

received :: IrcMessage -> LB ()
received msg = do s   <- get
                  case M.lookup (command msg) (ircCallbacks s) of
                    Just cbs -> allCallbacks (map snd cbs) msg
                    _        -> return ()

-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: Message a => [a -> LB ()] -> a -> LB ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg = do
    handleIrc (liftIO . putStrLn . ("Main: caught (and ignoring) "++). show) (f msg)
    allCallbacks fs msg

------------------------------------------------------------------------
