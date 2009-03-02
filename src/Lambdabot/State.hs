{-# LANGUAGE ScopedTypeVariables #-}
-- | Support for the LB (LambdaBot) monad
module Lambdabot.State (
        -- ** Functions to access the module's state
        readMS, withMS, modifyMS, writeMS,
        accessorMS,

        -- ** Utility functions for modules that need state for each target.
        GlobalPrivate(global), mkGlobalPrivate, withPS, readPS, withGS, readGS,
        writePS, writeGS,

        -- * more LB support
        forkLB, liftLB
  ) where

import Lambdabot
import Lambdabot.Util            (withMWriter, timeout)

import Control.Concurrent (forkIO, readMVar, modifyMVar_, newMVar, MVar, ThreadId)
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)

import Lambdabot.Message (Nick)

-- withMWriter :: MVar a -> (a -> (a -> IO ()) -> IO b) -> IO b
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
withMS :: (ModuleState mod -> (ModuleState mod -> LB ()) -> LB a) -> ModuleT mod LB a
withMS f = do
    ref <- getRef
    lift . lbIO $ \conv -> withMWriter ref $ \x writer ->
        conv $ f x (liftIO . writer)

-- | Read the module's private state.
readMS :: ModuleT mod LB (ModuleState mod)
readMS = getRef >>= liftIO . readMVar

-- | Produces a with-function. Needs a better name.
accessorMS :: (ModuleState mod -> (t, t -> ModuleState mod)) ->
  (t -> (t -> LB ()) -> LB a) -> ModuleT mod LB a
accessorMS decompose f = withMS $ \s writer ->
  let (t,k) = decompose s in f t (writer . k)

-- | Modify the module's private state.
modifyMS :: (ModuleState mod -> ModuleState mod) -> ModuleT mod LB ()
modifyMS f = getRef >>= liftIO . flip modifyMVar_ (return . f)

-- | Write the module's private state. Try to use withMS instead.
writeMS :: ModuleState mod -> ModuleT mod LB ()
writeMS (x :: s) = modifyMS . const $ x     -- need to help out 6.5

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
withPS :: ModuleState mod ~ GlobalPrivate g p
  => Nick  -- ^ The target
  -> (Maybe p -> (Maybe p -> LB ()) -> LB a)
    -- ^ @Just x@ writes x in the user's private state, @Nothing@ removes it.
  -> ModuleT mod LB a
withPS who f = do
  mvar <- accessPS return id who
  lift . lbIO $ \conv -> withMWriter mvar $ \x writer ->
      conv $ f x (liftIO . writer)

-- | Reads private state.
readPS :: ModuleState mod ~ GlobalPrivate g p
  => Nick -> ModuleT mod LB (Maybe p)
readPS = accessPS (liftIO . readMVar) (\_ -> return Nothing)

-- | Reads private state, executes one of the actions success and failure
-- which take an MVar and an action producing a @Nothing@ MVar, respectively.
accessPS :: ModuleState mod ~ GlobalPrivate g p
  => (MVar (Maybe p) -> LB a) -> (LB (MVar (Maybe p)) -> LB a) -> Nick
  -> ModuleT mod LB a
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
withGS :: ModuleState mod ~ GlobalPrivate g p
  => (g -> (g -> LB ()) -> LB ()) -> ModuleT mod LB ()
withGS f = withMS $ \state writer ->
  f (global state) $ \g -> writer $ state { global = g }

-- | Reads global state.
readGS :: ModuleState mod ~ GlobalPrivate g p
  => ModuleT mod LB g
readGS = global `fmap` readMS


-- The old interface, as we don't wanna be too fancy right now.
writePS :: ModuleState mod ~ GlobalPrivate g p
  => Nick -> Maybe p -> ModuleT mod LB ()
writePS who x = withPS who (\_ writer -> writer x)

writeGS :: ModuleState mod ~ GlobalPrivate g p
  => g -> ModuleT mod LB ()
writeGS g = withGS (\_ writer -> writer g)

-- | run an IO action in another thread, with a timeout, lifted into LB
forkLB :: LB a -> LB ThreadId
forkLB f = (`liftLB` f) $ \g -> do
            forkIO $ do
                timeout (15 * 1000 * 1000) g
                return ()

-- | lift an io transformer into LB
liftLB :: (IO a -> IO b) -> LB a -> LB b
liftLB f = LB . mapReaderT f . runLB -- lbIO (\conv -> f (conv lb))
