{-# LANGUAGE TypeFamilies #-}
-- | Support for the LB (LambdaBot) monad
module Lambdabot.State (
        -- ** Functions to access the module's state
        MonadLBState(..),
        readMS, modifyMS, writeMS,
        accessorMS,

        -- ** Utility functions for modules that need state for each target.
        GlobalPrivate(global), mkGlobalPrivate, withPS, readPS, withGS, readGS,
        writePS, writeGS
  ) where

import {-# SOURCE #-} Lambdabot.Monad
import Lambdabot.Util       (withMWriter)

import Control.Concurrent   (readMVar, newMVar, MVar)
import Control.Monad        (liftM)
import Control.Monad.Trans  (liftIO)

import Lambdabot.Message    (Nick)

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
    withMS :: (LBState m -> (LBState m -> LB ()) -> LB a) -> m a

-- | Read the module's private state.
readMS :: MonadLBState m => m (LBState m)
readMS = withMS (\st _ -> return st)

-- | Produces a with-function. Needs a better name.
accessorMS :: MonadLBState m => (LBState m -> (t, t -> LBState m)) ->
  (t -> (t -> LB ()) -> LB a) -> m a
accessorMS decompose f = withMS $ \s writer ->
  let (t,k) = decompose s in f t (writer . k)

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
  lbIO $ \conv -> withMWriter mvar $ \x writer ->
      conv $ f x (liftIO . writer)

-- | Reads private state.
readPS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => Nick -> m (Maybe p)
readPS = accessPS (liftIO . readMVar) (\_ -> return Nothing)

-- | Reads private state, executes one of the actions success and failure
-- which take an MVar and an action producing a @Nothing@ MVar, respectively.
accessPS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => (MVar (Maybe p) -> LB a) -> (LB (MVar (Maybe p)) -> LB a)
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
  => (g -> (g -> LB ()) -> LB ()) -> m ()
withGS f = withMS $ \state writer ->
  f (global state) $ \g -> writer $ state { global = g }

-- | Reads global state.
readGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => m g
readGS = global `liftM` readMS


-- The old interface, as we don't wanna be too fancy right now.
writePS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => Nick -> Maybe p -> m ()
writePS who x = withPS who (\_ writer -> writer x)

writeGS :: (MonadLBState m, LBState m ~ GlobalPrivate g p)
  => g -> m ()
writeGS g = withGS (\_ writer -> writer g)
