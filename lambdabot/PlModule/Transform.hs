{-# OPTIONS -fglasgow-exts #-}
module PlModule.Transform (
    transform, optimize, alphaRename {- for testing -},
  ) where

import PlModule.Common
import PlModule.Rules
import PlModule.PrettyPrinter
import qualified PlModule.Set as S
--import qualified Data.Set as S

import Data.Graph
import qualified Map as M
import Control.Monad.State

import Data.List hiding (union)

type Set = S.Set

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

class OrdMonadPlus m where
  cut     ::          m a -> m a
  isMZero ::          m a -> Bool
  nubM    :: Ord a => m a -> m a
  returnO ::          a -> m a
  extO    :: Ord b => (a -> m b) -> (m a -> m b)
  fmapO   :: Ord b => (a -> b) -> (m a -> m b)
  mplusO  :: Ord a => m a -> m a -> m a
  mzeroO  ::          m a
  msumO   :: Ord a => [m a] -> m a

instance OrdMonadPlus S.Set where
  cut x = case S.elems x of
    []    -> S.empty
    (y:_) -> S.singleton y
  isMZero = S.null
  nubM = id
  returnO = S.singleton
  extO f x = S.unions (f `map` S.elems x)
  fmapO f x = S.fromList (f `map` S.elems x)
  mzeroO = S.empty
  mplusO = S.union
  msumO = S.unions

{-
instance OrdMonadPlus [] where
  cut = take 1
  isMZero = null
  nubM = nub
  returnO = return
  extO = (=<<)
  fmapO = fmap
  mzeroO = mzero
  mplusO = mplus
  msumO  = msum
-}

toOrdMonadPlus :: OrdMonadPlus m => Maybe a -> m a
toOrdMonadPlus Nothing = mzeroO
toOrdMonadPlus (Just x)= returnO x

-- Missing in the libs
comparing :: (Ord b) => (a -> b) -> (a -> a -> Ordering)
comparing p x y = compare (p x) (p y)

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
  adjust (Var _ str) -- | Just n <- readM str = log (n*n+1) / 4
                     | str == "uncurry"    = -4
		     | str == "s"          = 5
                     | str == "flip"       = 0.1
		     | str == ">>="        = 0.05
		     | str == "$"          = 0.01
		     | str == "subtract"   = 0.01
                     | str == "ap"         = 3
                     | str == "return"     = -2
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
    let chn = let ?first = True in boundedStep (snd t)
        chnn = let ?first = False in S.unions . map boundedStep $ S.elems chn
        new = minimumBy (comparing fst) . map (sizeExpr' &&& id) . S.elems $ 
	        (snd t `S.insert` chn) `S.union` chnn
    guard $ fst new < fst t
    return new

boundedStep :: (?first :: Bool) => Expr -> Set Expr
boundedStep e = red (step e) where
  mx = 32
  mx' = mx - 1

  red xs | len <= mx = xs
--         | trace (show len) False = bt
         | otherwise = S.fromList 
	     [S.elems xs !! ((j*(len-1))`div`mx') | j <- [0..mx']]
    where len = S.size xs

step :: (?first :: Bool) => Expr -> Set Expr
step e = msumO $ (\r -> rewrite r e) `map` rules
 
rewrite :: (?first :: Bool, OrdMonadPlus m) => RewriteRule -> Expr -> m Expr
rewrite rl e :: m a = case rl of
    Up r1 r2     -> let e'  :: m a = cut $ rewrite r1 e
			e'' :: m a = rewrite r2 `extO` e'
		    in if isMZero e'' then e' else e''
    OrElse r1 r2 -> let e'  :: m a = rewrite r1 e
                    in if isMZero e' then rewrite r2 e else e' 
    Then r1 r2   -> rewrite r2 `extO` nubM (rewrite r1 e)
    Opt  r       -> returnO e `mplusO` rewrite r e
    If   p  r    -> if isMZero (rewrite p e :: m a) then mzeroO else rewrite r e
    Hard r       -> if ?first then rewrite r e else mzeroO
    _            -> rewDeep rl e
    
  where -- rew = ...; rewDeep = ...

rewDeep :: (?first :: Bool, OrdMonadPlus m) => RewriteRule -> Expr -> m Expr
rewDeep rule e = rew rule e `mplusO` case e of
    Var _ _    -> mzeroO
    Lambda _ _ -> error "lambda: optimizer only works for closed expressions"
    Let _ _    -> error "let: optimizer only works for closed expressions"
    App e1 e2  -> ((`App` e2) `fmapO` rewDeep rule e1) `mplusO` 
		  ((e1 `App`) `fmapO` rewDeep rule e2)

rew :: (?first :: Bool, OrdMonadPlus m) => RewriteRule -> Expr -> m Expr
rew (RR r1 r2) e = toOrdMonadPlus $ fire r1 r2 e 
rew (CRR r) e = toOrdMonadPlus $ r e
rew (Or rs) e = msumO $ map (\x -> rew x e) rs
rew (Down r1 r2) e :: m a 
  = if isMZero e'' then e' else e'' where
    e'  :: m a = cut $ rew r1 e
    e'' :: m a = rewDeep (r2) `extO` e'
rew r@(Then   {}) e = rewrite r e
rew r@(OrElse {}) e = rewrite r e
rew r@(Up     {}) e = rewrite r e
rew r@(Opt    {}) e = rewrite r e
rew r@(If     {}) e = rewrite r e
rew r@(Hard   {}) e = rewrite r e

