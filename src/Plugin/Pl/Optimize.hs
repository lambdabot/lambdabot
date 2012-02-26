{-# LANGUAGE ImplicitParams #-}
module Plugin.Pl.Optimize (
    optimize,
  ) where

import Plugin.Pl.Common
import Plugin.Pl.Rules
import Plugin.Pl.PrettyPrinter

import Data.List (nub)
import Data.Maybe (listToMaybe)
import Control.Monad.State

cut :: [a] -> [a]
cut = take 1

toMonadPlus :: MonadPlus m => Maybe a -> m a
toMonadPlus Nothing = mzero
toMonadPlus (Just x)= return x

type Size = Double
-- | The 'size' of an expression, lower is better
--
-- This seems to be a better size for our purposes,
-- despite being "a little" slower because of the wasteful uglyprinting
sizeExpr' :: Expr -> Size
sizeExpr' e = fromIntegral (length $ show e) + adjust e where
  -- hackish thing to favor some expressions if the length is the same:
  -- (+ x) --> (x +)
  -- x >>= f --> f =<< x
  -- f $ g x --> f (g x)
  adjust :: Expr -> Size
  adjust (Var _ str) -- Just n <- readM str = log (n*n+1) / 4
                     | str == "uncurry"    = -4
--                     | str == "s"          = 5
                     | str == "flip"       = 0.1
                     | str == ">>="        = 0.05
                     | str == "$"          = 0.01
                     | str == "subtract"   = 0.01
                     | str == "ap"         = 2
                     | str == "liftM2"     = 1.01
                     | str == "return"     = -2
                     | str == "zipWith"    = -4
                     | str == "const"      = 0 -- -2
                     | str == "fmap"       = -1
  adjust (Lambda _ e') = adjust e'
  adjust (App e1 e2)  = adjust e1 + adjust e2
  adjust _ = 0

-- | Optimize an expression
optimize :: Expr -> [Expr]
optimize e = result where
  result :: [Expr]
  result = map (snd . fromJust) . takeWhile isJust .
    iterate (>>= simpleStep) $ Just (sizeExpr' e, e)

  simpleStep :: (Size, Expr) -> Maybe (Size, Expr)
  simpleStep t = do
    let chn  = let ?first = True  in step (snd t)
        chnn = let ?first = False in step =<< chn
        new  = filter (\(x,_) -> x < fst t) . map (sizeExpr' &&& id) $
                snd t: chn ++ chnn
    listToMaybe new

-- | Apply all rewrite rules once
step :: (?first :: Bool) => Expr -> [Expr]
step e = nub $ rewrite rules e

-- | Apply a single rewrite rule
--
rewrite :: (?first :: Bool) => RewriteRule -> Expr -> [Expr]
rewrite rl e = case rl of
    Up r1 r2     -> let e'  = cut $ rewrite r1 e
                        e'' = rewrite r2 =<< e'
                    in if null e'' then e' else e''
    OrElse r1 r2 -> let e'  = rewrite r1 e
                    in if null e' then rewrite r2 e else e'
    Then r1 r2   -> rewrite r2 =<< nub (rewrite r1 e)
    Opt  r       -> e: rewrite r e
    If   p  r    -> if null (rewrite p e) then mzero else rewrite r e
    Hard r       -> if ?first then rewrite r e else mzero
    Or rs        -> (\x -> rewrite x e) =<< rs
    RR {}        -> rewDeep rl e
    CRR {}       -> rewDeep rl e
    Down {}      -> rewDeep rl e

  where -- rew = ...; rewDeep = ...

-- Apply a 'deep' reqrite rule
rewDeep :: (?first :: Bool) => RewriteRule -> Expr -> [Expr]
rewDeep rule e = rew rule e `mplus` case e of
    Var _ _    -> mzero
    Lambda _ _ -> error "lambda: optimizer only works for closed expressions"
    Let _ _    -> error "let: optimizer only works for closed expressions"
    App e1 e2  -> ((`App` e2) `map` rewDeep rule e1) `mplus`
                  ((e1 `App`) `map` rewDeep rule e2)

-- | Apply a rewrite rule to an expression
--   in a 'deep' position, i.e. from inside a RR,CRR or Down
rew :: (?first :: Bool) => RewriteRule -> Expr -> [Expr]
rew (RR r1 r2)   e = toMonadPlus $ fire r1 r2 e
rew (CRR r)      e = toMonadPlus $ r e
rew (Or rs)      e = (\x -> rew x e) =<< rs
rew (Down r1 r2) e = if null e'' then e' else e''
  where
    e'  = cut $ rew r1 e
    e'' = rewDeep r2 =<< e'
rew r@(Then   {}) e = rewrite r e
rew r@(OrElse {}) e = rewrite r e
rew r@(Up     {}) e = rewrite r e
rew r@(Opt    {}) e = rewrite r e
rew r@(If     {}) e = rewrite r e
rew r@(Hard   {}) e = rewrite r e
