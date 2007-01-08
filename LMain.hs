module LMain where

import Shared
import Lambdabot
import Message
import IRC
import IRCBase
import Config
import Control.Monad(when)
import Network(PortID(PortNumber))

import qualified Data.Map as M

import System.Environment

import Data.Maybe
import Control.Monad.State (get, liftIO, modify)

data Args = Args { privileged :: Bool,
                   initcmds   :: [String],
                   runOnline  :: Bool }

main' :: Maybe DynLoad -> (LB (), [String]) -> IO ()
main' dyn (loadStaticModules, pl) = do
    x    <- getArgs
    let a = parseArgs x
    case a of
        Just args -> runIrc (initcmds args) loadStaticModules
                     (if runOnline args then onlineMain else offlineMain (privileged args)) ld pl
        _         -> putStrLn "Usage: lambdabot [--online|--restricted|-e 'cmd']*"

    where ld = fromMaybe (error "no dynamic loading") dyn
          parseArgs ("--online":x)     = parseArgs x >>$ \a -> a { runOnline = True }
          parseArgs ("--restricted":x) = parseArgs x >>$ \a -> a { privileged = False }
          parseArgs ("-e":cmd:x)       = parseArgs x >>$ \a -> a { initcmds = cmd : initcmds a }
          parseArgs []                 = Just $ Args { privileged = True, initcmds = [], runOnline = False }
          parseArgs _                  = Nothing
          (>>$) = flip fmap


------------------------------------------------------------------------

onlineMain :: LB ()
onlineMain = online "freenode" (Config.host Config.config) (PortNumber $ fromIntegral $ Config.port Config.config)
             (nName $ Config.name Config.config) (Config.userinfo Config.config) received

offlineMain :: Bool -> LB ()
offlineMain cmdline = do 
  when cmdline $ modify (\st -> let privUsers  = ircPrivilegedUsers st
                                    privUsers'| cmdline   = M.insert (Nick "freenode" "null") True privUsers
                                              | otherwise = privUsers
                                in st { ircPrivilegedUsers = privUsers' })
  offline "freenode" received

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
