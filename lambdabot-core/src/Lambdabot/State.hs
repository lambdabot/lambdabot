{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | Support for the LB (LambdaBot) monad
module Lambdabot.State
    ( -- ** Functions to access the module's state
      MonadLBState(..)
    , readMS
    , writeMS
    , modifyMS
    
    -- ** Utility functions for modules that need state for each target.
    , GlobalPrivate -- (global)
    , mkGlobalPrivate
    
    , withPS
    , readPS
    , writePS
    
    , withGS
    , readGS
    , writeGS
    
    -- ** Handling global state
    , readGlobalState
    , writeGlobalState
  ) where

import Lambdabot.File
import Lambdabot.Logging
import Lambdabot.Monad
import Lambdabot.Module
import Lambdabot.Nick
import Lambdabot.Command
import Lambdabot.Util
import Lambdabot.Util.Serial

import Control.Concurrent.Lifted
import Control.Exception.Lifted as E
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as P
import Data.IORef.Lifted
import qualified System.IO as I
import qualified System.FilePath as I
import qualified System.Directory as I

-- | Thread-safe modification of an MVar.
withMWriter :: MonadBaseControl IO m => MVar a -> (a -> (a -> m ()) -> m b) -> m b
withMWriter mvar f = bracket
  (do x <- takeMVar mvar; ref <- newIORef x; return (x,ref))
  (\(_,ref) -> tryPutMVar mvar =<< readIORef ref)
  (\(x,ref) -> f x $ writeIORef ref)

class MonadLB m => MonadLBState m where
    type LBState m
    
    -- | Update the module's private state.
    -- This is the preferred way of changing the state. The state will be locked
    -- until the body returns. The function is exception-safe, i.e. even if
    -- an error occurs or the thread is killed (e.g. because it deadlocked and
    -- therefore exceeded its time limit), the state from the last write operation
    -- will be restored. If the writer escapes, calling it will have no observable
    -- effect.
    -- @withMS@ is not composable, in the sense that a readMS from within the body
    -- will cause a dead-lock. However, all other possibilies to access the state
    -- that came to my mind had even more serious deficiencies such as being prone
    -- to race conditions or semantic obscurities.
    withMS :: (LBState m -> (LBState m -> m ()) -> m a) -> m a

instance MonadLB m => MonadLBState (ModuleT st m) where
    type LBState (ModuleT st m) = st
    withMS f = do
        ref <- asks moduleState
        withMWriter ref f

instance MonadLBState m => MonadLBState (Cmd m) where
    type LBState (Cmd m) = LBState m
    withMS f = do
        x <- liftWith $ \run -> 
            withMS $ \st wr -> 
                run (f st (lift . wr))
        restoreT (return x)

-- | Read the module's private state.
readMS :: MonadLBState m => m (LBState m)
readMS = withMS (\st _ -> return st)

-- | Modify the module's private state.
modifyMS :: MonadLBState m => (LBState m -> LBState m) -> m ()
modifyMS f = withMS $ \st wr -> wr (f st)

-- | Write the module's private state. Try to use withMS instead.
writeMS :: MonadLBState m => LBState m -> m ()
writeMS = modifyMS . const

-- | This datatype allows modules to conviently maintain both global
--   (i.e. for all clients they're interacting with) and private state.
--   It is implemented on top of readMS\/withMS.
--
-- This simple implementation is linear in the number of private states used.
data GlobalPrivate g p = GP {
  global :: !g,
  private :: ![(Nick,MVar (Maybe p))],
  maxSize :: Int
}

-- | Creates a @GlobalPrivate@ given the value of the global state. No private
--   state for clients will be created.
mkGlobalPrivate :: Int -> g -> GlobalPrivate g p
mkGlobalPrivate ms g = GP {
  global = g,
  private = [],
  maxSize = ms
}

-- Needs a better interface. The with-functions are hardly useful.
-- | Writes private state. For now, it locks everything.
withPS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => Nick  -- ^ The target
  -> (Maybe p -> (Maybe p -> LB ()) -> LB a)
    -- ^ @Just x@ writes x in the user's private state, @Nothing@ removes it.
  -> m a
withPS who f = do
  mvar <- accessPS return id who
  lb $ withMWriter mvar f

-- | Reads private state.
readPS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => Nick -> m (Maybe p)
readPS = accessPS (liftIO . readMVar) (\_ -> return Nothing)

-- | Reads private state, executes one of the actions success and failure
-- which take an MVar and an action producing a @Nothing@ MVar, respectively.
accessPS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => (MVar (Maybe p) -> m a) -> (m (MVar (Maybe p)) -> m a)
  -> Nick
  -> m a
accessPS success failure who = withMS $ \state writer ->
  case lookup who $ private state of
    Just mvar -> do
      let newPrivate = (who,mvar):
            filter ((/=who) . fst) (private state)
      length newPrivate `seq` writer (state { private = newPrivate })
      success mvar
    Nothing -> failure $ do
      mvar <- liftIO $ newMVar Nothing
      let newPrivate = take (maxSize state) $ (who,mvar): private state
      length newPrivate `seq` writer (state { private = newPrivate })
      return mvar

-- | Writes global state. Locks everything
withGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => (g -> (g -> m ()) -> m ()) -> m ()
withGS f = withMS $ \state writer ->
  f (global state) $ \g -> writer $ state { global = g }

-- | Reads global state.
readGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => m g
readGS = fmap global readMS


-- The old interface, as we don't wanna be too fancy right now.
writePS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => Nick -> Maybe p -> m ()
writePS who x = withPS who (\_ writer -> writer x)

writeGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => g -> m ()
writeGS g = withGS (\_ writer -> writer g)

-- ---------------------------------------------------------------------
--
-- Handling global state
--

-- | Peristence: write the global state out
writeGlobalState :: ModuleT st LB ()
writeGlobalState = do
    m     <- asks theModule
    mName <- asks moduleName
    
    debugM ("saving state for module " ++ show mName)
    case moduleSerialize m of
        Nothing  -> return ()
        Just ser -> do
            state' <- readMS
            case serialize ser state' of
                Nothing  -> return ()   -- do not write any state
                Just out -> do
                    stateFile <- lb (findLBFileForWriting mName)
                    (stateFile', hdl) <- io (I.openBinaryTempFileWithDefaultPermissions (I.takeDirectory stateFile) (I.takeFileName stateFile))
                    io (P.hPutStr hdl out >> I.hClose hdl)
                    io (I.renameFile stateFile' stateFile)

-- | Read it in
readGlobalState :: Module st -> String -> LB (Maybe st)
readGlobalState module' name = do
    debugM ("loading state for module " ++ show name)
    case moduleSerialize module' of
        Just ser -> do
            mbStateFile <- findLBFileForReading name
            case mbStateFile of
                Nothing         -> return Nothing
                Just stateFile  -> io $ do
                    state' <- Just `fmap` P.readFile stateFile `E.catch` \SomeException{} -> return Nothing
                    E.catch (evaluate $ maybe Nothing (Just $!) (deserialize ser =<< state')) -- Monad Maybe)
                        (\e -> do
                            errorM $ "Error parsing state file for: "
                                ++ name ++ ": " ++ show (e :: SomeException)
                            errorM $ "Try removing: "++ show stateFile
                            return Nothing) -- proceed regardless
        Nothing -> return Nothing
