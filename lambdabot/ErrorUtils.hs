
module ErrorUtils where

import Monad
import Control.Monad.Error (MonadError (..))

catchErrorJust :: MonadError e m => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchErrorJust decide m handler
 = catchError m (\e -> case decide e of
                         Just b -> handler b
                         Nothing -> throwError e)

handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

handleErrorJust :: MonadError e m => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleErrorJust decide = flip $ catchErrorJust decide

tryError :: MonadError e m => m a -> m (Either e a)
tryError m = catchError (liftM Right m) (return . Left)

tryErrorJust :: MonadError e m => (e -> Maybe b) -> m a -> m (Either b a)
tryErrorJust decide m = catchErrorJust decide (liftM Right m) (return . Left)

finallyError :: MonadError e m => m a -> m b -> m a
finallyError m after = do a <- catchError m (\e -> after >> throwError e)
                          after
                          return a

bracketError :: MonadError e m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketError before after m
 = do v <- before
      finallyError (m v) (after v)

bracketError_ :: MonadError e m => m a -> m b -> m c -> m c
bracketError_ before after m = bracketError before (\_ -> after) (\_ -> m)
