--
-- | Let's go lambdabot!
--
module Main where

import Shared
import Lambdabot
import Config
import Modules
import Message

import qualified Data.Map as M

import System.Environment

import Data.Maybe
import Control.Monad.State (get, liftIO)

------------------------------------------------------------------------

-- do argument handling
main :: IO ()
main = main' Nothing

dynmain :: DynLoad  -> IO ()
dynmain fn = main' (Just fn)

main' :: Maybe DynLoad -> IO ()
main' dyn = do
    x    <- getArgs
    case x of
        ["--online"] -> runIrc Online  loadStaticModules onlineMain  load
        []           -> runIrc Offline loadStaticModules offlineMain load
        _            -> putStrLn "Usage: lambdabot [--online]"

    where load = fromMaybe (error "no dynamic loading") dyn

--
-- special online target for ghci use
online :: IO ()
online = runIrc Online loadStaticModules onlineMain $
            fromMaybe (error "no dynamic loading") Nothing

------------------------------------------------------------------------

onlineMain :: LB ()
onlineMain = serverSignOn (protocol config) (name config) (userinfo config) >> mainloop

offlineMain :: LB ()
offlineMain = mainloop

------------------------------------------------------------------------

-- it's all asynchronous, remember, the reader and writer threads
-- communicating over chans in the LB state. maybe its too much?
mainloop :: LB ()
mainloop = do
    mmsg <- ircRead
    case mmsg of
        Nothing -> return ()
        Just msg -> do
            s   <- get
            case M.lookup (command msg) (ircCallbacks s) of
                 Just cbs -> allCallbacks (map snd cbs) msg
                 _        -> return ()
    mainloop

-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: Message a => [a -> LB ()] -> a -> LB ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg = do
    handleIrc (liftIO . putStrLn . ("Main: caught (and ignoring) "++). show) (f msg)
    allCallbacks fs msg

------------------------------------------------------------------------
