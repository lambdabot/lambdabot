module LMain where

import Shared
import Lambdabot
import Config
import Message

import qualified Data.Map as M

import System.Environment

import Data.Maybe
import Control.Monad.State (get, liftIO, modify)


main' :: Maybe DynLoad -> (LB (), [String]) -> IO ()
main' dyn (loadStaticModules, pl) = do
    x    <- getArgs
    case x of
        ["--online"]     -> runIrc Online  loadStaticModules onlineMain  ld pl 
        ["--restricted"] -> runIrc Offline loadStaticModules (offlineMain False) ld pl
        []               -> runIrc Offline loadStaticModules (offlineMain True)  ld pl
        _                -> putStrLn "Usage: lambdabot [--online|--restricted]"

    where ld = fromMaybe (error "no dynamic loading") dyn


------------------------------------------------------------------------

onlineMain :: LB ()
onlineMain = serverSignOn (protocol config) (name config) (userinfo config) >> mainloop

offlineMain :: Bool -> LB ()
offlineMain cmdline = do
  modify (\st -> let privUsers  = ircPrivilegedUsers st
                     privUsers'| cmdline   = M.insert (Nick "freenode" "null") True privUsers
                               | otherwise = privUsers
                 in st { ircPrivilegedUsers = privUsers' })
  mainloop

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
