module MoreModule where
-- 	$Id: MoreModule.hs,v 1.2 2003/07/25 13:19:22 eleganesh Exp $	
import IRC
import Control.Monad.State
import Data.FiniteMap
import Data.IORef

newtype MoreModule = MoreModule ()

theModule = MODULE moreModule
moreModule = MoreModule ()

instance Module MoreModule where
    moduleName   m = return "more"
    moduleSticky m = False
    commands     m = return ["more"]
    process      m msg target cmd rest
      = do 
        maybemyref <- gets (\s -> lookupFM (ircModuleState s) "more")
        case maybemyref of
            Just myref -> 
                do modstate <- liftIO (readIORef myref)
		   liftIO (writeIORef myref (ModuleState rest))
		   ircPrivmsg target (stripMS modstate)
	    -- init state for this module if it doesn't exist
	    Nothing ->
		do liftLB $ moduleInit m
		   process m msg target cmd rest
    moduleInit   m = 
        do s <- get
           newRef <- liftIO . newIORef $ ModuleState [""]
           let stateMap = ircModuleState s
           put (s { ircModuleState = 
                    addToFM stateMap "more" newRef })
                    
