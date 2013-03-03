-- | Error utilities
module Lambdabot.Util.Error
    ( bracketError
    , finallyError
    ) where

import Control.Monad.Error (MonadError (..))

-- | 'finallyError' is a monadic version of the classic UNWIND-PROTECT of
--   lisp fame. Given parameters m and after (both monads) we proceed to
--   work on m. If an error is caught, we execute the out-guard, after,
--   before rethrowing the error. If m does not fail, after is executed
--   and the value of m is returned.
finallyError :: MonadError e m => m a -- ^ Monadic operation
             -> m b -- ^ Guard
             -> m a -- ^ Returns: A new monad.
finallyError m after = do a <- catchError m (\e -> after >> throwError e)
                          _ <- after
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
