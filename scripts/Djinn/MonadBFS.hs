module MonadBFS(MonadBFS(..), BFS, runBFS) where
import Control.Monad

class MonadPlus m => MonadBFS m where
    mwrap :: m a -> m a

data Tree a = Tip a | Fork (BFS a)
    deriving (Show)
newtype BFS a = BFS { unBFS :: [Tree a] }
    deriving (Show)

instance Functor BFS where
    fmap f (BFS xs) = BFS $ map tmap xs
      where  tmap (Tip a) = Tip (f a)
             tmap (Fork x) = Fork (fmap f x)

instance Monad BFS where
    return a = BFS [Tip a]
    fs >>= f = fjoin $ fmap f fs

fjoin :: BFS (BFS a) -> BFS a
fjoin = BFS . concatMap tjoin . unBFS
  where tjoin (Tip x) = unBFS x
        tjoin (Fork x) = [Fork $ fjoin x]

instance MonadPlus BFS where
    mzero = BFS []
    BFS f1 `mplus` BFS f2 = BFS $  f1 ++ f2

instance MonadBFS BFS where
    mwrap x = BFS [Fork x]

type Q a = ([a],[a])
mtq :: Q a
mtq = ([], [])
enq :: [a] -> Q a -> Q a
enq as ([], []) = (reverse as, [])
enq as (f, r) = (f, as ++ r)
deq :: Q a -> Maybe (a, Q a)
deq ([], []) = Nothing
deq ([a], r) = Just (a, (reverse r, []))
deq (a:f, r) = Just (a, (f, r))
deq _ = error "deq"

runBFS :: BFS a -> [a]
runBFS fr = run (enq [fr] mtq)
  where run aq =
            case deq aq of
            Nothing -> []
            Just (BFS xs, q) ->
                [x | Tip x <- xs] ++ 
                run (enq [ f | Fork f <- xs] q)
