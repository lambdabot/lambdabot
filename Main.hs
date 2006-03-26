--
-- | Let's go lambdabot!
--
module Main where

import Shared
import Lambdabot
import qualified IRC
import Config
import Modules
import qualified Map as M

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
        _            -> runIrc Offline loadStaticModules offlineMain load

    where load = fromMaybe (error "no dynamic loading") dyn

------------------------------------------------------------------------

onlineMain :: LB ()
onlineMain = ircSignOn (name config) (userinfo config) >> mainloop

offlineMain :: LB ()
offlineMain = mainloop

------------------------------------------------------------------------

mainloop :: LB ()
mainloop = do 
    msg <- ircRead
    s   <- get
    case M.lookup (msgCommand msg) (ircCallbacks s) of
         Just cbs -> allCallbacks (map snd cbs) msg
         _        -> return ()
    mainloop

-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: [IRC.Message -> LB ()] -> IRC.Message -> LB ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg = do
    handleIrc (liftIO . putStrLn) (f msg)
    allCallbacks fs msg

------------------------------------------------------------------------
