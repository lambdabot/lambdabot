module MonadException where
-- 	$Id: MonadException.hs,v 1.3 2003/07/28 14:44:56 eleganesh Exp $

import Prelude hiding (catch)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

#if __GLASGOW_HASKELL__ >= 600
#else
throwIO = ioError
#endif

-- would do this using MonadError, but instance MonadError IOError IO exists
-- and the fundep prevents us making MonadError Exception IO

class Monad m => MonadException m where
-- note: I'm not 100% sure about the correctness of my defaults!

-- minimal complete definition: throwM, catchM, evaluateM, blockM, unblockM

  throwM :: Exception -> m a

  catchM :: m a -> (Exception -> m a) -> m a

  catchMJust :: (Exception -> Maybe b) -> m a -> (b -> m a) -> m a
  catchMJust decide comp handler
   = catchM comp (\e -> case decide e of
                          Just b -> handler b
                          Nothing -> throwM e)

  handleM :: (Exception -> m a) -> m a -> m a
  handleM handler comp = catchM comp handler

  handleMJust :: (Exception -> Maybe b) -> (b -> m a) -> m a -> m a
  handleMJust decide handler comp = catchMJust decide comp handler

  tryM :: m a -> m (Either Exception a)
  tryM comp = catchM (liftM Right comp) (return . Left)

  tryMJust :: (Exception -> Maybe b) -> m a -> m (Either b a)
  tryMJust decide comp = catchMJust decide (liftM Right comp) (return . Left)

  evaluateM :: a -> m a
  blockM :: m a -> m a
  unblockM :: m a -> m a

  finallyM :: m a -> m b -> m a
  finallyM comp after 
    = blockM $
         do a <- catchM (unblockM comp) (\e -> after >> throwM e)
            after
            return a


  bracketM :: m a -> (a -> m b) -> (a -> m c) -> m c
  bracketM before after comp 
    = blockM $
         do v <- before
            a <- catchM (unblockM (comp v)) (\e -> after v >> throwM e)
            after v
            return a

  bracketM_ :: m a -> m b -> m c -> m c
  bracketM_ before after comp = bracketM before (\_ -> after) (\_ -> comp)


instance MonadException IO where
  throwM = throwIO
  catchM = catch
  catchMJust = catchJust
  handleM = handle
  handleMJust = handleJust
  tryM = try
  tryMJust = tryJust

  evaluateM = evaluate
  blockM = block
  unblockM = unblock

  finallyM = finally
  bracketM = bracket
  bracketM_ = bracket_

instance MonadException m => MonadException (StateT s m) where
  throwM e = StateT $ \s -> throwM e

  catchM (StateT c) handler
   = StateT $ \s -> catchM (c s) (\e -> runStateT (handler e) s)

  evaluateM v = StateT $ \s -> evaluateM (v,s)

  blockM (StateT c) = StateT $ \s -> blockM (c s)
  unblockM (StateT c) = StateT $ \s -> unblockM (c s)


instance MonadException m => MonadException (ReaderT r m) where
  throwM e = ReaderT $ \r -> throwM e

  catchM (ReaderT c) handler
   = ReaderT $ \r -> catchM (c r) (\e -> runReaderT (handler e) r)

  evaluateM v = ReaderT $ \_ -> evaluateM v

  blockM (ReaderT c) = ReaderT $ \r -> blockM (c r)
  unblockM (ReaderT c) = ReaderT $ \r -> unblockM (c r)

