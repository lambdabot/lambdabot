{-# OPTIONS -fvia-C -O2 -optc-O3 #-}
module Plugin.Pl.Transform (
    transform, optimize,
  ) where

import Plugin.Pl.Common
import Plugin.Pl.Rules
import Plugin.Pl.PrettyPrinter

import Data.List (nub)
import qualified Data.Map as M

import Data.Graph (stronglyConnComp, flattenSCC, flattenSCCs)
import Control.Monad.State

{-
nub :: Ord a => [a] -> [a]
nub = nub' S.empty where
  nub' _ [] = []
  nub' set (x:xs)
    | x `S.member` set = nub' set xs
    | otherwise = x: nub' (x `S.insert` set) xs
-}

occursP :: String -> Pattern -> Bool
occursP v (PVar v') = v == v'
occursP v (PTuple p1 p2) = v `occursP` p1 || v `occursP` p2
occursP v (PCons  p1 p2) = v `occursP` p1 || v `occursP` p2

freeIn :: String -> Expr -> Int
freeIn v (Var _ v') = fromEnum $ v == v'
freeIn v (Lambda pat e) = if v `occursP` pat then 0 else freeIn v e
freeIn v (App e1 e2) = freeIn v e1 + freeIn v e2
freeIn v (Let ds e') = if v `elem` map declName ds then 0 
  else freeIn v e' + sum [freeIn v e | Define _ e <- ds]

isFreeIn :: String -> Expr -> Bool
isFreeIn v e = freeIn v e > 0

tuple :: [Expr] -> Expr
tuple es  = foldr1 (\x y -> Var Inf "," `App` x `App` y) es

tupleP :: [String] -> Pattern
tupleP vs = foldr1 PTuple $ PVar `map` vs

dependsOn :: [Decl] -> Decl -> [Decl]
dependsOn ds d = [d' | d' <- ds, declName d' `isFreeIn` declExpr d]
  
unLet :: Expr -> Expr
unLet (App e1 e2) = App (unLet e1) (unLet e2)
unLet (Let [] e) = unLet e
unLet (Let ds e) = unLet $
  (Lambda (tupleP $ declName `map` dsYes) (Let dsNo e)) `App`
    (fix' `App` (Lambda (tupleP $ declName `map` dsYes)
                        (tuple  $ declExpr `map` dsYes)))
    where
  comps = stronglyConnComp [(d',d',dependsOn ds d') | d' <- ds]
  dsYes = flattenSCC $ head comps
  dsNo = flattenSCCs $ tail comps
  
unLet (Lambda v e) = Lambda v (unLet e)
unLet (Var f x) = Var f x

type Env = M.Map String String

-- It's a pity we still need that for the pointless transformation.
-- Otherwise a newly created id/const/... could be bound by a lambda
-- e.g. transform' (\id x -> x) ==> transform' (\id -> id) ==> id
alphaRename :: Expr -> Expr
alphaRename e = alpha e `evalState` M.empty where
  alpha :: Expr -> State Env Expr
  alpha (Var f v) = do fm <- get; return $ Var f $ maybe v id (M.lookup v fm)
  alpha (App e1 e2) = liftM2 App (alpha e1) (alpha e2)
  alpha (Let _ _) = assert False bt
  alpha (Lambda v e') = inEnv $ liftM2 Lambda (alphaPat v) (alpha e')

  -- act like a reader monad
  inEnv :: State s a -> State s a
  inEnv (State f) = State $ \s -> (fst $ f s, s)

  alphaPat (PVar v) = do
    fm <- get
    let v' = "$" ++ show (M.size fm)
    put $ M.insert v v' fm
    return $ PVar v'
  alphaPat (PTuple p1 p2) = liftM2 PTuple (alphaPat p1) (alphaPat p2)
  alphaPat (PCons p1 p2) = liftM2 PCons (alphaPat p1) (alphaPat p2)


transform :: Expr -> Expr
transform = transform' . alphaRename . unLet

transform' :: Expr -> Expr
transform' (Let {}) = assert False bt
transform' (Var f v) = Var f v
transform' (App e1 e2) = App (transform' e1) (transform' e2)
transform' (Lambda (PTuple p1 p2) e) 
  = transform' $ Lambda (PVar "z") $ 
      (Lambda p1 $ Lambda p2 $ e) `App` f `App` s where
    f = Var Pref "fst" `App` Var Pref "z"
    s = Var Pref "snd" `App` Var Pref "z"
transform' (Lambda (PCons p1 p2) e) 
  = transform' $ Lambda (PVar "z") $ 
      (Lambda p1 $ Lambda p2 $ e) `App` f `App` s where
    f = Var Pref "head" `App` Var Pref "z"
    s = Var Pref "tail" `App` Var Pref "z"
transform' (Lambda (PVar v) e) = transform' $ getRidOfV e where
  getRidOfV (Var f v') | v == v'   = id'
                       | otherwise = const' `App` Var f v'
  getRidOfV l@(Lambda pat _) = assert (not $ v `occursP` pat) $ 
    getRidOfV $ transform' l
  getRidOfV (Let {}) = assert False bt
  getRidOfV e'@(App e1 e2) 
    | fr1 && fr2 = scomb `App` getRidOfV e1 `App` getRidOfV e2
    | fr1 = flip' `App` getRidOfV e1 `App` e2
    | Var _ v' <- e2, v' == v = e1
    | fr2 = comp `App` e1 `App` getRidOfV e2
    | True = const' `App` e'
    where
      fr1 = v `isFreeIn` e1
      fr2 = v `isFreeIn` e2

cut :: [a] -> [a]
cut = take 1

toMonadPlus :: MonadPlus m => Maybe a -> m a
toMonadPlus Nothing = mzero
toMonadPlus (Just x)= return x

type Size = Double
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

optimize :: Expr -> [Expr]
optimize e = result where
  result :: [Expr]
  result = map (snd . fromJust) . takeWhile isJust . 
    iterate ((=<<) simpleStep) $ Just (sizeExpr' e, e)

  simpleStep :: (Size, Expr) -> Maybe (Size, Expr)
  simpleStep t = do 
    let chn = let ?first = True in step (snd t)
        chnn = let ?first = False in step =<< chn
        new = filter (\(x,_) -> x < fst t) . map (sizeExpr' &&& id) $ 
                snd t: chn ++ chnn
    case new of
      [] -> Nothing
      (new':_) -> return new'

step :: (?first :: Bool) => Expr -> [Expr]
step e = nub $ rewrite rules e
 
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

rewDeep :: (?first :: Bool) => RewriteRule -> Expr -> [Expr]
rewDeep rule e = rew rule e `mplus` case e of
    Var _ _    -> mzero
    Lambda _ _ -> error "lambda: optimizer only works for closed expressions"
    Let _ _    -> error "let: optimizer only works for closed expressions"
    App e1 e2  -> ((`App` e2) `map` rewDeep rule e1) `mplus`
                  ((e1 `App`) `map` rewDeep rule e2)

rew :: (?first :: Bool) => RewriteRule -> Expr -> [Expr]
rew (RR r1 r2) e = toMonadPlus $ fire r1 r2 e 
rew (CRR r) e = toMonadPlus $ r e
rew (Or rs) e = (\x -> rew x e) =<< rs
rew (Down r1 r2) e
  = if null e'' then e' else e'' where
    e'  = cut $ rew r1 e
    e'' = rewDeep r2 =<< e'
rew r@(Then   {}) e = rewrite r e
rew r@(OrElse {}) e = rewrite r e
rew r@(Up     {}) e = rewrite r e
rew r@(Opt    {}) e = rewrite r e
rew r@(If     {}) e = rewrite r e
rew r@(Hard   {}) e = rewrite r e
