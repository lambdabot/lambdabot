
module ExceptionError where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error (MonadError (..))
import MonadException

#if __GLASGOW_HASKELL__ < 603
import Control.Monad.Trans
#endif

-- ExceptionErrorT differs from Control.Monad.Error.ErrorT in two ways:
-- (1) it doesn't implement the "fail" operation of Monad or the "mzero"
--     operation of MonadPlus - as things stand, I would prefer to leave
--     monad failures (e.g. from pattern matching) to the same mechanisms
--     that deal with other functional code errors, and I have no use for
--     MonadPlus. This could of course change.
-- (2) It catches Exceptions thrown from an underlying MonadException and
--     rethrows them as its own error type. The rationale for this is that
--     we might have a stack of transformers, with IO at the bottom. Since
--     IO handles the control flow transfer of Exceptions, if we have a
--     StateT in the stack, its state will be unwound right back to the
--     point that the Exception is caught. Sometimes this is what is wanted,
--     but sometimes it isn't.

class ExceptionError e where
  fromException :: Exception -> e


newtype ExceptionErrorT e m a
 = ExceptionErrorT { runExceptionErrorT :: m (Either e a) }

translateException :: (MonadException m,ExceptionError e)
                   => m a -> ExceptionErrorT e m a
translateException m = ExceptionErrorT $
                        do res <- tryM m
                           case res of
                             Left e -> return $ Left $ fromException e
                             Right v -> return $ Right v

instance (MonadException m,ExceptionError e) => Monad (ExceptionErrorT e m)
 where
  return v = translateException $ return v
  m >>= f = ExceptionErrorT $
              do res <- tryM $
                  do res' <- runExceptionErrorT m
                     case res' of
                       Left e -> return $ Left e
                       Right v -> runExceptionErrorT $ f v
                 case res of
                  Left e -> return $ Left $ fromException e
                  Right v -> return v

instance Functor m => Functor (ExceptionErrorT e m) where
  f `fmap` ExceptionErrorT m = ExceptionErrorT $ fmap f `fmap` m

instance (MonadException m,ExceptionError e)
      => MonadError e (ExceptionErrorT e m)
 where
  throwError e = ExceptionErrorT $ return $ Left e
  catchError m handler = ExceptionErrorT $
                           do res <- runExceptionErrorT m
                              case res of
                                 Left e -> runExceptionErrorT $ handler e
                                 Right v -> return $ Right v


instance (MonadException m,MonadReader r m,ExceptionError e)
      => MonadReader r (ExceptionErrorT e m)
 where
  ask = translateException ask
  local f m = ExceptionErrorT $ local f $ runExceptionErrorT m

instance (MonadException m,MonadState s m,ExceptionError e)
      => MonadState s (ExceptionErrorT e m)
 where
  get = translateException get
  put s = translateException $ put s

instance (MonadException m,MonadIO m,ExceptionError e)
      => MonadIO (ExceptionErrorT e m)
 where
  liftIO m = translateException $ liftIO m
