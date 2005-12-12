-- Intuitionistic theorem prover
-- Written by Roy Dyckhoff, Summer 1991    
-- Modified to use the LWB syntax  Summer 1997
-- and simplified in various ways...
--
-- Translated to Haskell by Lennart Augustsson December 2005
--
-- Incorporates the Vorob'ev-Hudelmaier etc calculus (I call it LJT)
-- See RD's paper in JSL 1992:
-- "Contraction-free calculi for intuitionistic logic"
--
-- Torkel Franzen (at SICS) gave me good ideas about how to write this 
-- properly, taking account of first-argument indexing,
-- and I learnt a trick or two from Neil Tennant's "Autologic" book.

module LJT(module LJTFormula, provable,
	prove, Proof) where
import List(partition, mapAccumL)
import Monad

import LJTFormula

import Debug.Trace
mtrace :: String -> a -> a
mtrace m x = if debug then trace m x else x
debug :: Bool
debug = False

provable :: Formula -> Bool
provable a = not $ null $ prove [] a 

prove :: [(Symbol, Formula)] -> Formula -> [Proof]
prove env a = runP $ redtop env a

redtop :: [(Symbol, Formula)] -> Formula -> P Proof
redtop ifs a = do
    let form = foldr (:->) a (map snd ifs)
    p <- redant [] [] [] [] form
    nf (foldl Apply p (map (Var . fst) ifs))

------------------------------
----- 
type Proof = Term

subst :: Term -> Symbol -> Term -> P Term
subst b x term = sub term
  where sub t@(Var s') = if x == s' then copy [] b else return t
	sub (Lam s t) = liftM1 (Lam s) (sub t)
	sub (Apply t1 t2) = liftM2 Apply (sub t1) (sub t2)
	sub t = return t

copy :: [(Symbol, Symbol)] -> Term -> P Term
copy r (Var s) = return $ Var (lookupWithDefault r s s)
copy r (Lam s t) = do
    s' <- newSym "c"
    liftM1 (Lam s') $ copy ((s, s'):r) t
copy r (Apply t1 t2) = liftM2 Apply (copy r t1) (copy r t2)
copy _r t = return t

---

applyAtom :: Term -> Term -> Term
applyAtom f a = Apply f a

curryt :: Term -> Term
curryt p = Lam x $ Lam y $ Apply p (applys (Ctuple 2) [Var x, Var y])
  where x = Symbol "x"
	y = Symbol "y"

left :: Term -> Term
left p = Lam x $ Apply p (Apply Cleft (Var x))
  where x = Symbol "x"

right :: Term -> Term
right p = Lam x $ Apply p (Apply Cright (Var x))
  where x = Symbol "x"

applyImp :: Term -> Term -> Term
applyImp p q = Apply p (Apply q (Lam y $ Apply p (Lam x (Var y))))
  where x = Symbol "x"
	y = Symbol "y"

-- ((c->d)->false) -> ((c->false)->false, d->false)
-- p : (c->d)->false)
-- replace p1 and p2 with the components of the pair
cImpDImpFalse :: Symbol -> Symbol -> Term -> Term -> P Term
cImpDImpFalse p1 p2 cdf gp = do
    let p1b = Lam cf $ Apply cdf $ Lam x $ Apply Cabsurd $ Apply (Var cf) (Var x)
	p2b = Lam d $ Apply cdf $ Lam c $ Var d
	cf = Symbol "cf"
	x = Symbol "x"
	d = Symbol "d"
	c = Symbol "d"
    subst p1b p1 gp >>= subst p2b p2

-- More simplifications:
--  split where no variables used can be removed
--  either with equal RHS can me merged.

-- Compute the normal form
nf :: Term -> P Term
nf ee = spine ee []
  where spine (Apply f a) as = do a' <- nf a; spine f (a' : as)
	spine (Lam s e) [] = liftM1 (Lam s) (nf e)
	spine (Lam s e) (a : as) = do e' <- subst a s e; spine e' as
	spine (Csplit n) (b : tup : args) | istup && n <= length xs = spine (applys b xs) args
	  where (istup, xs) = getTup tup
		getTup (Ctuple _) = (True, [])
		getTup (Apply f a) = let (tf, as) = getTup f in (tf, a:as)
		getTup _ = (False, [])
	spine Ceither (l : _r : (Apply Cleft  x) : as) = spine (Apply l x) as
	spine Ceither (_l : r : (Apply Cright x) : as) = spine (Apply r x) as
        spine Cabsurd (Apply Cabsurd x : as) = spine Cabsurd (x : as)
	spine f as = return $ applys f as


------------------------------
----- Our Proof monad, P, a monad with state and multiple results

newtype P a = P { unP :: PS -> (PS, [a]) }

instance Monad P where
    return x = P $ \ s -> (s, [x])
    P m >>= f = P $ \ s ->
	case m s of
	(s', xs) ->
	    case mapAccumL (\ st x -> unP (f x) st) s' xs of
	    (s'', ys) -> (s'', concat ys)

instance Functor P where
    fmap f (P m) = P $ \ s ->
        case m s of
	(s', xs) -> (s', map f xs)

-- The state, just an integer for generating new variables
data PS = PS !Integer
startPS :: PS
startPS = PS 1

nextInt :: P Integer
nextInt = P $ \ (PS i) -> (PS (i+1), [i])

none :: P a
none = P $ \ s -> (s, [])

many :: [a] -> P a
many xs = P $ \ s -> (s, xs)

runP :: P a -> [a]
runP (P m) =
    case m startPS of
    (_, xs) -> xs


------------------------------
----- Atomic formulae

data AtomF = AtomF Term Symbol
    deriving (Eq)
instance Show AtomF where
    show (AtomF p s) = show p ++ ":" ++ show s

type AtomFs = [AtomF]

findAtom :: Symbol -> AtomFs -> Maybe Term
findAtom s atoms =
    case [ p | AtomF p s' <- atoms, s == s' ] of
	[] -> Nothing
	p : _ -> Just p

------------------------------
----- Implications of one atom

data AtomImp = AtomImp Symbol Antecedents
     deriving (Show)
type AtomImps = [AtomImp]

extract :: AtomImps -> Symbol -> ([Antecedent], AtomImps)
extract aatomImps@(atomImp@(AtomImp a' bs) : atomImps) a =
    case compare a a' of
    GT -> let (rbs, restImps) = extract atomImps a in (rbs, atomImp : restImps)
    EQ -> (bs, atomImps)
    LT -> ([], aatomImps)
extract _ _ = ([], [])

insert :: AtomImps -> AtomImp -> AtomImps
insert [] ai = [ ai ]
insert aatomImps@(atomImp@(AtomImp a' bs') : atomImps) ai@(AtomImp a bs) =
    case compare a a' of
    GT -> atomImp : insert atomImps ai
    EQ -> AtomImp a (bs ++ bs') : atomImps
    LT -> ai : aatomImps

------------------------------
----- Nested implications, (a -> b) -> c

data NestImp = NestImp Term Formula Formula Formula -- NestImp a b c represents (a :-> b) :-> c
instance Show NestImp where
    show (NestImp _ a b c) = show $ (a :-> b) :-> c

type NestImps = [NestImp]


------------------------------
----- Ordering of nested implications
heuristics :: Bool
heuristics = True

order :: NestImps -> Formula -> AtomImps -> NestImps
order nestImps g atomImps =
    if heuristics then
	nestImps
    else
	let 
	    good_for (NestImp _ _ _ Falsity) = True
	    good_for (NestImp _ _ _ g') = g == g'
	    nice_for (NestImp _ _ _ (PVar s)) =
	        case extract atomImps s of
	        (bs', _) -> let bs = [ b | A _ b <- bs'] in g `elem` bs || Falsity `elem` bs
	    nice_for _ = False
	    (good, ok) = partition good_for nestImps
	    (nice, bad) = partition nice_for ok
	in  good ++ nice ++ bad

------------------------------
----- Generate a new unique variable
newSym :: String -> P Symbol
newSym pre = do
   i <- nextInt
   return $ Symbol $ pre ++ show i

{-
newVar :: String -> P Term
newVar pre = do
   s <- newSym pre
   return $ Var s
-}

------------------------------
----- Generate all ways to select one element of a list
select :: [a] -> P (a, [a])
select zs = many [ del n zs | n <- [0 .. length zs - 1] ]
  where del 0 (x:xs) = (x, xs)
	del n (x:xs) = let (y,ys) = del (n-1) xs in (y, x:ys)
	del _ _ = error "select"

------------------------------
----- 

-- redant atoms, nestImps, atomImps

data Antecedent = A Term Formula deriving (Show)
type Antecedents = [Antecedent]

redant :: Antecedents -> AtomImps -> NestImps -> AtomFs -> Formula -> P Proof
redant as atomImps nestImps atoms gg =
    case as of
    [] -> redsucc gg
    a:l -> redant1 a l gg
  where redant0 l g = redant l atomImps nestImps atoms g
	redant1 :: Antecedent -> Antecedents -> Formula -> P Proof
	redant1 a l g = 
	    mtrace ("redant1 " ++ show ((a, l), atomImps, nestImps, atoms, g)) $
	    redant1' a l g

	redant1' :: Antecedent -> Antecedents -> Formula -> P Proof
	redant1' (A p (PVar a)) _  (PVar a') | a == a' = return p
	redant1' (A p a'@(PVar s)) l g | a' /= g =
	    let a = AtomF p s
	        (bs, restAtomImps) = extract atomImps s
	    in  redant ([A (Apply f p) b | A f b <- bs] ++ l) restAtomImps nestImps (a : atoms) g
	redant1' (A _ Truth) l g = redant0 l g
	redant1' (A p Falsity) _ _ = return $ Apply Cabsurd p
	redant1' (A p (Conj bs)) l g = do
	   vs <- mapM (const (newSym "v")) bs
	   gp <- redant0 (zipWith (\ v a -> A (Var v) a) vs bs ++ l) g
	   return $ applys (Csplit (length bs)) [foldr Lam gp vs, p]
	redant1' (A p (a :| b)) l g = do
	   vl <- newSym "l"
	   vr <- newSym "r"
	   pl <- redant1 (A (Var vl) a) l g
	   pr <- redant1 (A (Var vr) b) l g
	   return $ applys Ceither [Lam vl pl, Lam vr pr, p]
	redant1' (A p (a :-> b)) l g = redantimp p a b l g
	redant1' _a _l _g = none

	redantimp :: Term -> Formula -> Formula -> Antecedents -> Formula -> P Proof
	-- p : PVar s -> b
	redantimp p (PVar s) b l g = redantimpatom p s b l g
	-- p : Truth -> b
	redantimp p Truth b l g = redant1 (A (Apply p Cunit) b) l g
	-- p : Falsity -> b, no help, ignore
	redantimp _ Falsity _ l g = redant0 l g
	-- p : (c & d) -> b
	redantimp _ (Conj []) _ _ _ = error "redantimp"
	redantimp p (Conj (c : cs)) b l g = do
	    x <- newSym "x"
	    let d = case cs of [a] -> a; _ -> Conj cs
	    gp <- redantimp (Var x) c (d :-> b) l g
	    subst (curryt p) x gp
	-- p : (c | d) -> b
	redantimp p (c :| d) b l g = do
	    x <- newSym "x"
	    y <- newSym "y"
	    gp <- redantimp (Var x) c b (A (Var y) (d :-> b) : l) g
	    subst (right p) y gp >>= subst (left p) x
	-- p : (c -> d) -> b
	redantimp p (c :-> d) b l g = redantimpimp p c d b l g

	redantimpimp :: Term -> Formula -> Formula -> Formula -> Antecedents -> Formula -> P Proof
	-- next clause exploits ~(C->D) <=> (~~C & ~D)
	-- which isn't helpful when D = false
	redantimpimp p c d Falsity l g | d /= Falsity = do
	    x <- newSym "x"
	    y <- newSym "y"
	    gp <- redantimpimp (Var x) c Falsity Falsity (A (Var y) (d :-> Falsity) : l) g
	    cImpDImpFalse x y p gp
	-- p : (c -> d) -> b
	redantimpimp p c d b l g = redant l atomImps (NestImp p c d b : nestImps) atoms g

	redantimpatom :: Term -> Symbol -> Formula -> Antecedents -> Formula -> P Proof
	redantimpatom p s b l g =
	    case findAtom s atoms of
		Just a -> do
		    x <- newSym "x"
		    gp <- redant1 (A (Var x) b) l g
		    subst (applyAtom p a) x gp
		Nothing -> redant l (insert atomImps (AtomImp s [A p b])) nestImps atoms g

	redsucc :: Formula -> P Proof
	redsucc g =
	    mtrace ("redsucc " ++ show (g, atomImps, nestImps, atoms)) $
	    redsucc' g

	redsucc' :: Formula -> P Proof
	redsucc' Truth = return Cunit
	redsucc' Falsity = inconsis atomImps nestImps atoms
	redsucc' a@(PVar s) =
	    case findAtom s atoms of
	    Just p -> return p
	    Nothing ->
	        if posin s atomImps nestImps then
	            redsucc_choice a
		else
		    none
	redsucc' (Conj cs) = do
	    ps <- mapM redsucc cs
	    return $ applys (Ctuple (length cs)) ps
	-- next clause deals with succedent (A v B) by pushing the
	-- non-determinism into the treatment of implication on the left
	redsucc' (a :| b) = do
	    s1 <- newSym "_"
	    let v = PVar s1
	    redant0 [A Cleft $ a :-> v, A Cright $ b :-> v ] v
	redsucc' (a :-> b) = do
	    s <- newSym "x"
	    p <- redant1 (A (Var s) a) [] b
	    return $ Lam s p

	-- Now we have the hard part; maybe lots of formulae 
	-- of form (C->D)->B  in Imps to choose from!
	-- Which one to take first? We need some heuristics:
	-- first we take those where the minor premiss is trivial;
	-- next we take those where B is an atom and B->G is in AtomImps
	-- next we try to choose B a disjunction (even if G isn't)
	-- finally we choose the others
	redsucc_choice :: Formula -> P Proof
	redsucc_choice g = do
	    let ordImps = order nestImps g atomImps
	    (NestImp p c d b, restImps) <- select ordImps
	    x <- newSym "x"
	    z <- newSym "z"
	    qz <- redant [A (Var z) $ d :-> b] atomImps restImps atoms (c :-> d)
	    gp <- redant [A (Var x) b] atomImps restImps atoms g
	    subst (applyImp p (Lam z qz)) x gp

-- Classical ATP utility
-- from now on, goal is always = false...
inconsis :: AtomImps -> NestImps -> AtomFs -> P Proof
inconsis atomImps (NestImp p c d b : rest) atoms = inconsis1 p c d b atomImps rest atoms
inconsis _ [] _ = none

inconsis1 :: Term -> Formula -> Formula -> Formula -> AtomImps -> NestImps -> AtomFs -> P Proof
-- p : (c -> d) -> Falsity
-- Try (c->false), since ((c->false)->(c->d))
inconsis1 p c _d Falsity atomImps nestImps atoms = do
    x <- newSym "x"
    fp <- redant [A (Var x) c] atomImps nestImps atoms Falsity
    let aImpD = Lam x $ Apply Cabsurd fp
    return $ Apply p aImpD
inconsis1 p c d b atomImps nestImps atoms = do
    -- so can exploit ~((C->D)->B) eq ~~(C->D) & ~B
    -- p : (c -> d) -> b
    -- x : (c -> d) -> false
    -- y : b
    x <- newSym "x"
    y <- newSym "y"
    cd <- newSym "cd"
    pf1 <- redant [A (Var x) $ (c :-> d) :-> Falsity] atomImps nestImps atoms Falsity
    pf2 <- redant [A (Var y) b] atomImps nestImps atoms Falsity
    -- bf : b -> false
    -- cdf : (c->d) -> false
    let bf = Lam y pf2
	cdf = Lam cd $ Apply (Apply p (Var cd)) bf
    subst cdf x pf1

posin :: Symbol -> AtomImps -> NestImps -> Bool
posin g atomImps nestImps = posin1 g atomImps || posin2 g [ (a :-> b) :-> c | NestImp _ a b c <- nestImps ]

posin1 :: Symbol -> AtomImps -> Bool
posin1 g atomImps = any (\ (AtomImp _ bs) -> posin2 g [ b | A _ b <- bs]) atomImps

posin2 :: Symbol -> [Formula] -> Bool
posin2 g bs = any (posin3 g) bs

posin3 :: Symbol -> Formula -> Bool
posin3 g (a :| b) = posin3 g a && posin3 g b
posin3 g (Conj as) = any (posin3 g) as
posin3 g (_ :-> b) = posin3 g b
posin3 _g Falsity = True
posin3 s (PVar s') = s == s'
posin3 _ _ = False

---------------------------

lookupWithDefault :: (Eq a) => [(a, b)] -> b -> a -> b
lookupWithDefault ab b a =
    case lookup a ab of
    Nothing -> b
    Just b' -> b'

liftM1 :: (Functor f) => (a -> b) -> f a -> f b
liftM1 = fmap

