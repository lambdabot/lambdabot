
module Main where

import IRC
import Config
import Modules
import qualified Map as M

import Control.Monad.State

main :: IO ()
main = runIrc ircInit ircMain
    where
        ircInit = loadStaticModules
        ircMain = ircSignOn (name config) (userinfo config) >> mainloop

mainloop :: IRC ()
mainloop = do 
        msg <- ircRead
        s   <- get
        case M.lookup (msgCommand msg) (ircCallbacks s) of
             Just cbs -> allCallbacks (map snd cbs) msg
             _        -> return ()
        mainloop

--
-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: [IRCMessage -> IRC ()] -> IRCMessage -> IRC ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg = do
        handleIrc (liftIO . putStrLn) (f msg)
        allCallbacks fs msg

