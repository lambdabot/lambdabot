{-# OPTIONS -cpp #-}
{-# OPTIONS -fallow-overlapping-instances#-}
-- ^ hack

module GhciModule
    ( ghciModule
    , theModule
    ) where

import qualified Map as M

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.FiniteMap
import Data.IORef
import Data.List ( isPrefixOf, isSuffixOf )
import System.IO

-- TODO
import System.Posix.Types
import System.Posix.Signals

import Shell ( launch )

import IRC

newtype GHCiModule = GHCiModule ()

ghciModule = GHCiModule ()

theModule = MODULE ghciModule

myStripMS x = (stripMS x) :: (ThreadId, Chan String, Chan String)

instance Module GHCiModule where
    moduleName _ = return "ghci"
    moduleSticky _ = False
    commands _ = return ["ghci"]
    moduleInit _ = do inChan <- liftIO $ newChan
		      outChan <- liftIO $ newChan
		      threadId <- liftIO $ forkIO $ (launchGHCi inChan outChan)
		      makeInitialState "ghci" (threadId, inChan, outChan)
    moduleExit _ =
	do mbEnvStateRef <- gets (\s -> M.lookup "ghci" (ircModuleState s))
	   case mbEnvStateRef
		of Nothing -> return ()
		   Just envStateRef ->
		       do envState  <- liftIO $ readIORef envStateRef
		          let (threadId, _, _) = myStripMS envState
			  liftIO $ putStrLn "Killing"
			  liftIO $ killThread threadId
    process _ _ target "ghci" line =
	do mbEnvStateRef <- gets (\s -> M.lookup "ghci" (ircModuleState s))
	   case mbEnvStateRef
		of Nothing -> ircPrivmsg target "Something bad happened"
		   Just envStateRef ->
		       do envState <- liftIO $ readIORef envStateRef
		          let (_, inChan, outChan) = myStripMS envState
		          liftIO $ writeChan inChan line
			  liftIO $ threadDelay 200000
			  liftIO $ putStrLn "fetching"
			  lines <- fetch outChan target []
			  ircPrivmsg target (unlines lines)
			  liftIO $ putStrLn "end fetch"
        where fetch outChan target ls =
		  do empty <- liftIO $ isEmptyChan outChan
		     if not empty
			then do l <- liftIO $ readChan outChan
				fetch outChan target (l:ls)
			else return (reverse ls)

launchGHCi :: Chan String -> Chan String -> IO ()
launchGHCi inChan outChan =
    do (_, e) <- launch (runner inChan outChan)
		     "/usr/bin/ghci" ["-fglasgow-exts"]
       print e
       launchGHCi inChan outChan

runner :: Chan String -> Chan String -> ProcessID 
       ->  Handle -> Handle -> Handle -> IO ()
runner inChan outChan pid stdin stdout stderr =
    handleJust asyncExceptions (\_ -> signalProcess sigKILL pid) $ 
	       do hGetLine stdout `loopUntil` ("done." `isSuffixOf`)
	          loop
    where loop =
	      do -- hPutChar stdin '\n'
		 hFlush stdin
		 inLine <- readChan inChan
		 print inLine
		 secure <- isSecure inLine
		 when secure $
		    do threadId <- forkIO timeoutKiller
		       hPutStrLn stdin (inLine)
		       hFlush stdin
		       putStrLn $ "Sent " ++ inLine
		       threadDelay 100000
		       forward stdout
		       forward stderr
		       putStrLn $ "End of forward"
		       killThread threadId
		 loop      
	  timeoutKiller = do threadDelay (2000000) -- 2 secs
			     signalProcess sigINT pid
	  forward h = do threadDelay 50
			 ready <- hReady h
			 when ready $
			      do l <- hGetLine h
				 putStrLn l
				 let ls = wrap 80 l
				 mapM_ (writeChan outChan) ls
				 forward h
	  dumpErrs = do ready <- hReady stderr
			when ready $
			     do l <- hGetLine stderr
				putStrLn l -- on terminal
	  isSecure inLine = return True {-
	      if ":" `isPrefixOf` inLine
	         then if ":t" `isPrefixOf` inLine
		         then do writeChan outChan "You can only use :t !"
				 return False
			 else return True
		 else do hPutStrLn stdin $ (showString ":t " . showString "("
				       . showString inLine) ")"
			 threadDelay 10
			 typeLine <- hGetLine stdout
			 print typeLine
			 typeLine' <- hGetLine stdout
			 print typeLine'
 			 if "IO" `elem` words typeLine
			    then do writeChan outChan
				       "Your not allowed to use IO functions"
				    return False
			    else return True -}

f `loopUntil` p = do x <- f
		     if p x then return x
                            else f `loopUntil` p

wrap :: Int -> String -> [String]
wrap column str = f str []
    where f ""  ls = reverse ls
          f str ls = let (l, rest) = splitAt column str
		      in f rest (l:ls)

chanTc :: TyCon
chanTc = mkTyCon "Chan"

#if __GLASGOW_HASKELL__ < 603
myTy = mkAppTy
#else
myTy = mkTyConApp
#endif

instance Typeable a => Typeable (Chan a) where
    typeOf chan = myTy chanTc [typeOf ((undefined :: Chan a -> a) chan)]

threadIdTc :: TyCon
threadIdTc = mkTyCon "ThreadId"

#if __GLASGOW_HASKELL__ < 603
instance Typeable ThreadId where
    typeOf _ = mkAppTy threadIdTc []
#endif
