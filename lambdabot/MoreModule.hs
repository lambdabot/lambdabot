module MoreModule where
-- 	$Id: MoreModule.hs,v 1.2 2003/07/25 13:19:22 eleganesh Exp $	
import IRC
import Control.Monad.State
import qualified Map as M
import Data.IORef

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE moreModule

moreModule :: MoreModule
moreModule = MoreModule ()

instance Module MoreModule where
    moduleName   _ = return "more"
    moduleSticky _ = False
    commands     _ = return ["more"]
    process      m msg target cmd rest
      = do 
        maybemyref <- gets (\s -> M.lookup "more" (ircModuleState s))
        case maybemyref of
            Just myref -> 
                do modstate <- liftIO (readIORef myref)
		   liftIO (writeIORef myref (ModuleState rest))
		   ircPrivmsg target (stripMS modstate)
	    -- init state for this module if it doesn't exist
	    Nothing ->
		do liftLB $ moduleInit m
		   process m msg target cmd rest
    moduleInit   _ = 
        do s <- get
           newRef <- liftIO . newIORef $ ModuleState [""]
           let stateMap = ircModuleState s
           put (s { ircModuleState = 
                    M.insert "more" newRef stateMap })
                    
