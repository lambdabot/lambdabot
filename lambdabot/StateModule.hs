
-- 	$Id: StateModule.hs,v 1.8 2003/07/29 13:03:02 eris Exp $

module StateModule where

import IRC
import qualified Map as M

import Maybe
import Control.Monad.State
import Data.IORef

newtype StateModule = StateModule ()

theModule :: MODULE
theModule = MODULE stateModule

stateModule :: StateModule
stateModule = StateModule ()

instance Module StateModule where
    moduleName   _ = return "state"
    moduleSticky _ = False
    moduleHelp _ _ = return "@state - we all know it's evil"
    commands     _ = return ["state"]
    moduleInit   _ =
	do s <- get
	   newRef <- liftIO . newIORef $ ModuleState "nothing yet"
	   let stateMap = ircModuleState s
           put (s { ircModuleState =
		    M.insert "state" newRef stateMap })
    process      m msg target cmd rest
      = do 
        maybemyref <- gets (\s -> M.lookup "state" (ircModuleState s))
        case maybemyref of
            Just myref -> 
                do modstate <- liftIO (readIORef myref)
		   liftIO (writeIORef myref (ModuleState rest))
		   ircPrivmsg target (stripMS modstate)
	    -- init state for this module if it doesn't exist
	    Nothing ->
		do liftLB $ moduleInit m
		   process m msg target cmd rest
