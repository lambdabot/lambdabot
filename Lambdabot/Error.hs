-- | Error utilities
module Lambdabot.Error where

import Control.Monad       (liftM)
import Control.Monad.Error (MonadError (..))

-- | 'catchErrorJust' is an error catcher for the Maybe type. As input is given
--   a deciding function, a monad and a handler. When an error is caught, the
--   decider is executed to decide if the error should be handled or not.
--   Then the handler is eventually called to handle the error.
catchErrorJust :: MonadError e m => (e -> Maybe b) -- ^ Decider function
               -> m a -- ^ Monad
               -> (b -> m a) -- ^ Handler function
               -> m a -- ^ Result: A monadic operation on type a
catchErrorJust decide m handler
 = catchError m (\e -> case decide e of
                         Just b -> handler b
                         Nothing -> throwError e)

-- | 'handleError' is the flipped version of 'catchError'.
handleError :: MonadError e m => (e -> m a) -- ^ Error handler
            -> m a -- ^ Monad
            -> m a -- ^ Resulting monad
handleError = flip catchError

-- | 'handleErrorJust' is the flipped version of 'catchErrorJust'.
handleErrorJust :: MonadError e m => (e -> Maybe b) -- ^ Decider
                -> (b -> m a) -- ^ Handler
                -> m a -- ^ Monad
                -> m a -- ^ Resulting Monad
handleErrorJust = flip . catchErrorJust

-- | 'tryError' uses Either to explicitly define the outcome of a
--   monadic operation. An error is caught and placed into Right,
--   whereas successful operation is placed into Left.
tryError :: MonadError e m => m a -- ^ Monad to operate on
         -> m (Either e a) -- ^ Returns: Explicit Either type
tryError m = catchError (liftM Right m) (return . Left)

-- | 'tryErrorJust' is the 'catchErrorJust' version of 'tryError'
--   given is a decider guarding whether or not the error should be
--   handled. The handler will always Right and no errors are Left'ed
--   through. If the decider returns Nothing, the error will be thrown
--   further up.
tryErrorJust :: MonadError e m => (e -> Maybe b) -- ^ Decider
             -> m a -- ^ Monad
             -> m (Either b a) -- ^ Returns: Explicit Either type
tryErrorJust decide m = catchErrorJust decide (liftM Right m) (return . Left)

-- | 'finallyError' is a monadic version of the classic UNWIND-PROTECT of
--   lisp fame. Given parameters m and after (both monads) we proceed to
--   work on m. If an error is caught, we execute the out-guard, after,
--   before rethrowing the error. If m does not fail, after is executed
--   and the value of m is returned.
finallyError :: MonadError e m => m a -- ^ Monadic operation
             -> m b -- ^ Guard
             -> m a -- ^ Returns: A new monad.
finallyError m after = do a <- catchError m (\e -> after >> throwError e)
                          after
                          return a

-- | 'bracketError' is the monadic version of DYNAMIC-WIND from Scheme
--   fame. Parameters are: before, after and m. before is the in-guard
--   being executed before m. after is the out-guard and protects fails
--   of the m.
--   In the Haskell world, this scheme is called a bracket and is often
--   seen employed to manage resources.
bracketError :: MonadError e m => m a -- ^ Before (in-guard) monad
             -> (a -> m b) -- ^ After (out-guard) operation. Fed output of before
             -> (a -> m c) -- ^ Monad to work on. Fed with output of before
             -> m c -- ^ Resulting monad.
bracketError before after m = do v <- before; finallyError (m v) (after v)

-- | 'bracketError_' is the non-bound version of 'bracketError'. The
--   naming scheme follows usual Haskell convention.
bracketError_ :: MonadError e m => m a -- ^ Before (in-guard)
              -> m b -- ^ After (out-guard)
              -> m c -- ^ Monad to work on
              -> m c -- ^ Resulting monad
bracketError_ before after m = bracketError before (\_ -> after) (\_ -> m)
