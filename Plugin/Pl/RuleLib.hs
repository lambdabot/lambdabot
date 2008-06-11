{-# OPTIONS_GHC -fglasgow-exts #-} -- fix this later
{-# LANGUAGE FlexibleInstances, PatternGuards #-}

-- | This marvellous module contributed by Thomas J\344ger
module Plugin.Pl.RuleLib
       (  -- Using rules
          RewriteRule(..), fire
       ,  -- Defining rules
          rr,rr0,rr1,rr2,up,down
       ) where

import Plugin.Pl.Common
import Plugin.Pl.Names

import Data.Array
import qualified Data.Set as S

import Control.Monad.Fix (fix)

-- Next time I do somthing like this, I'll actually think about the combinator
-- language before, instead of producing something ad-hoc like this:
data RewriteRule
  = RR     Rewrite Rewrite           -- ^ A 'Rewrite' rule, rewrite the first to the second
                                     --   'Rewrite's can contain 'Hole's
  | CRR    (Expr -> Maybe Expr)      -- ^ Haskell function as a rule, applied to subexpressions
  | Down   RewriteRule RewriteRule   -- ^ Like Up, but applied to subexpressions
  | Up     RewriteRule RewriteRule   -- ^ Apply the first rule, then try the second rule on the first result
                                     --   if it fails, returns the result of the first rule
  | Or     [RewriteRule]             -- ^ Use all rules
  | OrElse RewriteRule RewriteRule   -- ^ Try the first rule, if it fails use the second rule
  | Then   RewriteRule RewriteRule   -- ^ Apply the first rule, apply the second rule to the result
  | Opt    RewriteRule               -- ^ Optionally apply the rewrite rule, Opt x == Or [identity,x]
  | If     RewriteRule RewriteRule   -- ^ Apply the second rule only if the first rule has some results
  | Hard   RewriteRule               -- ^ Apply the rule only in the first pass

-- | An expression with holes to match or replace
data Rewrite = Rewrite {
  holes :: MExpr,  -- ^ Expression with holes
  rid   :: Int     -- ^ Number of holes
}

-- What are you gonna do when no recursive modules are possible?
class RewriteC a where
  getRewrite :: a -> Rewrite

instance RewriteC MExpr where
  getRewrite rule = Rewrite {
    holes = rule,
    rid   = 0
  }

-- lift functions to rewrite rules
instance RewriteC a => RewriteC (MExpr -> a) where
  getRewrite rule = Rewrite {
    holes = holes . getRewrite . rule . Hole $ pid,
    rid   = pid + 1
  } where
     pid = rid $ getRewrite (undefined :: a)


----------------------------------------------------------------------------------------
-- Applying/matching Rewrites

type ExprArr = Array Int Expr

-- | Fill in the holes in a 'MExpr'
myFire :: ExprArr -> MExpr -> MExpr
myFire xs (MApp e1 e2) = MApp (myFire xs e1) (myFire xs e2)
myFire xs (Hole h) = Quote $ xs ! h
myFire _ me = me

nub' :: Ord a => [a] -> [a]
nub' = S.toList . S.fromList

-- | Create an array, only if the keys in 'lst' are unique and all keys [0..n-1] are given
uniqueArray :: Ord v => Int -> [(Int, v)] -> Maybe (Array Int v)
uniqueArray n lst
  | length (nub' lst) == n = Just $ array (0,n-1) lst
  | otherwise = Nothing

-- | Try to match a Rewrite to an expression,
--   if there is a match, returns the expressions in the holes
match :: Rewrite -> Expr -> Maybe ExprArr
match (Rewrite hl rid') e  = uniqueArray rid' =<< matchWith hl e

-- | Fill in the holes in a 'Rewrite'
fire' :: Rewrite -> ExprArr -> MExpr
fire' (Rewrite hl _)   = (`myFire` hl)

fire :: Rewrite -> Rewrite -> Expr -> Maybe Expr
fire r1 r2 e = (fromMExpr . fire' r2) `fmap` match r1 e

-- | Match an Expr to a MExpr template, return the values used in the holes
matchWith :: MExpr -> Expr -> Maybe [(Int, Expr)]
matchWith (MApp e1 e2) (App e1' e2') =
  liftM2 (++) (matchWith e1 e1') (matchWith e2 e2')
matchWith (Quote e) e' = if e == e' then Just [] else Nothing
matchWith (Hole k) e = Just [(k,e)]
matchWith _ _ = Nothing

fromMExpr :: MExpr -> Expr
fromMExpr (MApp e1 e2)  = App (fromMExpr e1) (fromMExpr e2)
fromMExpr (Hole _)      = Var Pref "Hole" -- error "Hole in MExpr"
fromMExpr (Quote e)     = e

----------------------------------------------------------------------------------------
-- Difining rules

-- | Yet another pointless transformation:
--   Bring an MExpr to (more pointless) form by seeing it as a function
--     \hole_n -> ...
--   and writing that in pointless form
transformM :: Int -> MExpr -> MExpr
transformM _ (Quote e) = constE `a` Quote e
transformM n (Hole n') = if n == n' then idE else constE `a` Hole n'
transformM n (Quote (Var _ ".") `MApp` e1 `MApp` e2)
  | e1 `hasHole` n && not (e2 `hasHole` n)
  = flipE `a` compE `a` e2 `c` transformM n e1
transformM n e@(MApp e1 e2)
  | fr1 && fr2 = sE `a` transformM n e1 `a` transformM n e2
  | fr1        = flipE `a` transformM n e1 `a` e2
  | fr2, Hole n' <- e2, n' == n = e1
  | fr2        = e1 `c` transformM n e2
  | otherwise  = constE `a` e
  where
    fr1 = e1 `hasHole` n
    fr2 = e2 `hasHole` n

-- | Is there a (Hole n) in an expression?
hasHole :: MExpr -> Int -> Bool
hasHole (MApp e1 e2) n = e1 `hasHole` n || e2 `hasHole` n
hasHole (Quote _)    _ = False
hasHole (Hole n')    n = n == n'

-- | Variants of a rewrite rule: fill in (some of) the holes
--
-- haddock doesn't like n+k patterns, so rewrite them
--
getVariants, getVariants' :: Rewrite -> [Rewrite]
getVariants' r@(Rewrite _ 0)  = [r]
getVariants' r@(Rewrite e nk)
    | nk >= 1    = r : getVariants (Rewrite e' (nk-1))
    | otherwise  = error "getVariants' : nk went negative"
    where
        e' = decHoles $ transformM 0 e

        -- decrement all hole numbers
        decHoles (Hole n')    = Hole (n'-1)
        decHoles (MApp e1 e2) = decHoles e1 `MApp` decHoles e2
        decHoles me           = me

getVariants = getVariants' -- r = trace (show vs) vs where vs = getVariants' r

-- | Use this rewrite rule and rewrite rules derived from it by iterated
--   pointless transformation
rrList :: RewriteC a => a -> a -> [RewriteRule]
rrList r1 r2 = zipWith RR (getVariants r1') (getVariants r2') where
  r1' = getRewrite r1
  r2' = getRewrite r2

-- | Construct a 'RR' rewrite rule
rr, rr0, rr1, rr2 :: RewriteC a => a -> a -> RewriteRule
rr  r1 r2 = Or          $ rrList r1 r2
rr1 r1 r2 = Or . take 2 $ rrList r1 r2
rr2 r1 r2 = Or . take 3 $ rrList r1 r2
-- use only this rewrite rule, no variants
rr0 r1 r2 = RR r1' r2' where
  r1' = getRewrite r1
  r2' = getRewrite r2

-- | Apply Down/Up repeatedly
down, up :: RewriteRule -> RewriteRule
down = fix . Down
up   = fix . Up
