module Main where
--      $Id: Main.hs,v 1.28 2003/07/29 15:46:41 eleganesh Exp $      
import IRC
import GHC.IO
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.FiniteMap

import Util
import StaticModules
import BotConfig



main :: IO ()
main = runIrc ircInit ircMain

ircInit = loadStaticModules

ircMain :: IRC ()
ircMain
  = do  myname <- getMyname
        myuserinfo <- getMyuserinfo
        ircSignOn myname myuserinfo
        mainloop

mainloop :: IRC ()
mainloop
  = do  msg <- ircRead
        let cmd = msgCommand msg
        s <- get
        case lookupFM (ircCallbacks s) cmd of
             Just cbs -> allCallbacks cbs msg
             _ -> return ()
        mainloop

-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: [IRCMessage -> IRC ()] -> IRCMessage -> IRC ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg 
 = handleIrc (liftIO . putStrLn) (f msg) >> allCallbacks fs msg



-- Local Variables:
-- compile-command: "hmake -fglasgow-exts -package net -package HToolkit -package data Main && ./Main"
-- End:
