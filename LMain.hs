module LMain where

import Shared
import Lambdabot
import Message
import IRC
import IRCBase

import qualified Data.Map as M

import System.Environment

import Data.Maybe
import Control.Monad.State (get, liftIO, modify)


main' :: Maybe DynLoad -> (LB (), [String]) -> IO ()
main' dyn (loadStaticModules, pl) = do
    x    <- getArgs
    case x of
        ["--online"]     -> runIrc loadStaticModules onlineMain  ld pl 
        ["--restricted"] -> runIrc loadStaticModules (offlineMain False) ld pl
        []               -> runIrc loadStaticModules (offlineMain True)  ld pl
        _                -> putStrLn "Usage: lambdabot [--online|--restricted]"

    where ld = fromMaybe (error "no dynamic loading") dyn


------------------------------------------------------------------------

onlineMain :: IO (LB (), IrcMessage -> LB ())
onlineMain = online "freenode" received

offlineMain :: Bool -> IO (LB (), IrcMessage -> LB ())
offlineMain cmdline = do (loop, sendf) <- offline "freenode" received
                         return (modst >> loop, sendf)
    where modst = modify (\st -> let privUsers  = ircPrivilegedUsers st
                                     privUsers'| cmdline   = M.insert (Nick "freenode" "null") True privUsers
                                               | otherwise = privUsers
                                 in st { ircPrivilegedUsers = privUsers' })

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
