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

import Control.Monad.State (get, liftIO)

------------------------------------------------------------------------

main :: IO ()
main = main' Nothing

dynmain :: DynLoad  -> IO ()
dynmain fn = main' (Just fn)

main' :: Maybe DynLoad -> IO ()
main' Nothing   = runIrc ircInit ircMain (error "no dynamic loading")
main' (Just ld) = runIrc ircInit ircMain ld

------------------------------------------------------------------------

ircInit :: LB ()
ircInit = loadStaticModules

ircMain :: LB ()
ircMain = ircSignOn (name config) (userinfo config) >> mainloop

------------------------------------------------------------------------

mainloop :: LB ()
mainloop = do 
        msg <- ircRead
        s   <- get
        case M.lookup (msgCommand msg) (ircCallbacks s) of
             Just cbs -> allCallbacks (map snd cbs) msg
             _        -> return ()
        mainloop

-- TODO: Doesn't belong here
--
-- If an error reaches allCallbacks, then all we can sensibly do is
-- write it on standard out. Hopefully BaseModule will have caught it already
-- if it can see a better place to send it

allCallbacks :: [IRC.Message -> LB ()] -> IRC.Message -> LB ()
allCallbacks [] _ = return ()
allCallbacks (f:fs) msg = do
        handleIrc (liftIO . putStrLn) (f msg)
        allCallbacks fs msg

------------------------------------------------------------------------
