--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
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
import Data.List(partition)
import Control.Monad

import LJTFormula

import Debug.Trace
mtrace :: String -> a -> a
mtrace m x = if debug then trace m x else x
wrap :: (Show a, Show b) => String -> a -> b -> b
wrap fun args ret = mtrace (fun ++ ": " ++ show args) $
		    let o = show ret in seq o $ 
		    mtrace (fun ++ " returns: " ++ o) ret
wrapM :: (Show a, Show b, Monad m) => String -> a -> m b -> m b
wrapM fun args mret = do
    () <- mtrace (fun ++ ": " ++ show args) $ return ()
    ret <- mret
    () <- mtrace (fun ++ " returns: " ++ show ret) $ return ()
    return ret
debug :: Bool
debug = False

type MoreSolutions = Bool

provable :: Formula -> Bool
provable a = not $ null $ prove False [] a 

prove :: MoreSolutions -> [(Symbol, Formula)] -> Formula -> [Proof]
prove more env a = runP $ redtop more env a

redtop :: MoreSolutions -> [(Symbol, Formula)] -> Formula -> P Proof
redtop more ifs a = do
    let form = foldr (:->) a (map snd ifs)
    p <- redant more [] [] [] [] form
    nf (foldl Apply p (map (Var . fst) ifs))

------------------------------
----- 
type Proof = Term

subst :: Term -> Symbol -> Term -> P Term
subst b x term = sub term
  where sub t@(Var s') = if x == s' then copy [] b else return t
	sub (Lam s t) = liftM (Lam s) (sub t)
	sub (Apply t1 t2) = liftM2 Apply (sub t1) (sub t2)
	sub t = return t

copy :: [(Symbol, Symbol)] -> Term -> P Term
copy r (Var s) = return $ Var $ maybe s id $ lookup s r
copy r (Lam s t) = do
    s' <- newSym "c"
    liftM (Lam s') $ copy ((s, s'):r) t
copy r (Apply t1 t2) = liftM2 Apply (copy r t1) (copy r t2)
copy _r t = return t

------------------------------

-- XXX The symbols used in the functions below must not clash
-- XXX with any symbols from newSym.

applyAtom :: Term -> Term -> Term
applyAtom f a = Apply f a

curryt :: Int -> Term -> Term
curryt n p = foldr Lam (Apply p (applys (Ctuple n) (map Var xs))) xs
  where xs = [ Symbol ("x_" ++ show i) | i <- [0 .. n-1] ]

inj :: ConsDesc -> Int -> Term -> Term
inj cd i p = Lam x $ Apply p (Apply (Cinj cd i) (Var x))
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
    let p1b = Lam cf $ Apply cdf $ Lam x $ Apply (Ccases []) $ Apply (Var cf) (Var x)
	p2b = Lam d $ Apply cdf $ Lam c $ Var d
	cf = Symbol "cf"
	x = Symbol "x"
	d = Symbol "d"
	c = Symbol "c"
    subst p1b p1 gp >>= subst p2b p2

------------------------------

-- More simplifications:
--  split where no variables used can be removed
--  either with equal RHS can me merged.

-- Compute the normal form
nf :: Term -> P Term
nf ee = spine ee []
  where spine (Apply f a) as = do a' <- nf a; spine f (a' : as)
	spine (Lam s e) [] = liftM (Lam s) (nf e)
	spine (Lam s e) (a : as) = do e' <- subst a s e; spine e' as
	spine (Csplit n) (b : tup : args) | istup && n <= length xs = spine (applys b xs) args
	  where (istup, xs) = getTup tup
		getTup (Ctuple _) = (True, [])
		getTup (Apply f a) = let (tf, as) = getTup f in (tf, a:as)
		getTup _ = (False, [])
	spine (Ccases []) (e@(Apply (Ccases []) _) : as) = spine e as
	spine (Ccases cds) (Apply (Cinj _ i) x : as) | length as >= n = spine (Apply (as!!i) x) (drop n as)
		where n = length cds
	spine f as = return $ applys f as


------------------------------
----- Our Proof monad, P, a monad with state and multiple results

-- Note, this is the non-standard way to combine state with multiple
-- results.  But this is much better for backtracking.
newtype P a = P { unP :: PS -> [(PS, a)] }

instance Monad P where
    return x = P $ \ s -> [(s, x)]
    P m >>= f = P $ \ s ->
	[ y | (s',x) <- m s, y <- unP (f x) s' ]

instance Functor P where
    fmap f (P m) = P $ \ s ->
	[ (s', f x) | (s', x) <- m s ]

instance MonadPlus P where
    mzero = P $ \ _s -> []
    P fxs `mplus` P fys = P $ \ s -> fxs s ++ fys s

-- The state, just an integer for generating new variables
data PS = PS !Integer
startPS :: PS
startPS = PS 1

nextInt :: P Integer
nextInt = P $ \ (PS i) -> [(PS (i+1), i)]

none :: P a
none = mzero

many :: [a] -> P a
many xs = P $ \ s -> zip (repeat s) xs

atMostOne :: P a -> P a
atMostOne (P f) = P $ \ s -> take 1 (f s)

runP :: P a -> [a]
runP (P m) = map snd (m startPS)


------------------------------
----- Atomic formulae

data AtomF = AtomF Term Symbol
    deriving (Eq)
instance Show AtomF where
    show (AtomF p s) = show p ++ ":" ++ show s

type AtomFs = [AtomF]

findAtoms :: Symbol -> AtomFs -> [Term]
findAtoms s atoms = [ p | AtomF p s' <- atoms, s == s' ]

--removeAtom :: Symbol -> AtomFs -> AtomFs
--removeAtom s atoms = [ a | a@(AtomF _ s') <- atoms, s /= s' ]

addAtom :: AtomF -> AtomFs -> AtomFs
addAtom a as = if a `elem` as then as else a : as

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
    deriving (Eq)
instance Show NestImp where
    show (NestImp _ a b c) = show $ (a :-> b) :-> c

type NestImps = [NestImp]

addNestImp :: NestImp -> NestImps -> NestImps
addNestImp n ns = if n `elem` ns then ns else n : ns

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
	    good_for (NestImp _ _ _ (Disj [])) = True
	    good_for (NestImp _ _ _ g') = g == g'
	    nice_for (NestImp _ _ _ (PVar s)) =
	        case extract atomImps s of
	        (bs', _) -> let bs = [ b | A _ b <- bs'] in g `elem` bs || false `elem` bs
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

------------------------------
----- Generate all ways to select one element of a list
select :: [a] -> P (a, [a])
select zs = many [ del n zs | n <- [0 .. length zs - 1] ]
  where del 0 (x:xs) = (x, xs)
	del n (x:xs) = let (y,ys) = del (n-1) xs in (y, x:ys)
	del _ _ = error "select"

------------------------------
----- 

data Antecedent = A Term Formula deriving (Show)
type Antecedents = [Antecedent]

type Goal = Formula

--
-- This is the main loop of the proof search.
--
-- The redant functions reduce antecedents and the redsucc
-- function reduces the goal (succedent).
--
-- The antecedents are kept in four groups: Antecedents, AtomImps, NestImps, AtomFs
--   Antecedents contains as yet unclassified antecedents; the redant functions
--     go through them one by one and reduces and classifies them.
--   AtomImps contains implications of the form (a -> b), where `a' is an atom.
--     To speed up the processing it is stored as a map from the `a' to all the
--     formulae it implies.
--   NestImps contains implications of the form ((b -> c) -> d)
--   AtomFs contains atomic formulae.
--
-- There is also a proof object associated with each antecedent.
--
redant :: MoreSolutions -> Antecedents -> AtomImps -> NestImps -> AtomFs -> Goal -> P Proof
redant more antes atomImps nestImps atoms goal =
    wrapM "redant" (antes, atomImps, nestImps, atoms, goal) $
    case antes of
    [] -> redsucc goal
    a:l -> redant1 a l goal
  where redant0 l g = redant more l atomImps nestImps atoms g
	redant1 :: Antecedent -> Antecedents -> Goal -> P Proof
	redant1 a@(A p f) l g = 
	    wrapM "redant1" ((a, l), atomImps, nestImps, atoms, g) $
	    if f == g then
	        -- The goal is the antecedent, we're done.
	        -- XXX But we might want more?
	        if more then
		    return p `mplus` redant1' a l g
		else
		    return p
	    else
	        redant1' a l g

	-- Reduce the first antecedent
	redant1' :: Antecedent -> Antecedents -> Goal -> P Proof
	redant1' (A p (PVar s)) l g =
	   let af = AtomF p s
	       (bs, restAtomImps) = extract atomImps s
	   in  redant more ([A (Apply f p) b | A f b <- bs] ++ l) restAtomImps nestImps (addAtom af atoms) g
	redant1' (A p (Conj bs)) l g = do
	   vs <- mapM (const (newSym "v")) bs
	   gp <- redant0 (zipWith (\ v a -> A (Var v) a) vs bs ++ l) g
	   return $ applys (Csplit (length bs)) [foldr Lam gp vs, p]
	redant1' (A p (Disj ds)) l g = do
	   vs <- mapM (const (newSym "d")) ds
	   ps <- mapM (\ (v, (_, d)) -> redant1 (A (Var v) d) l g) (zip vs ds)
	   if null ds && g == Disj [] then
	       -- We are about to construct `void p : Void', so we shortcut
	       -- it with just `p'.
	       return p
	    else
	       return $ applys (Ccases (map fst ds)) (p : zipWith Lam vs ps)
	redant1' (A p (a :-> b)) l g = redantimp p a b l g

	redantimp :: Term -> Formula -> Formula -> Antecedents -> Goal -> P Proof
	redantimp t c d a g =
	    wrapM "redantimp" (c,d,a,g) $
	    redantimp' t c d a g

	-- Reduce an implication antecedent
	redantimp' :: Term -> Formula -> Formula -> Antecedents -> Goal -> P Proof
	-- p : PVar s -> b
	redantimp' p (PVar s) b l g = redantimpatom p s b l g
	-- p : (c & d) -> b
	redantimp' p (Conj cs) b l g = do
	    x <- newSym "x"
	    let imp = foldr (:->) b cs
	    gp <- redant1 (A (Var x) imp) l g
	    subst (curryt (length cs) p) x gp
	-- p : (c | d) -> b
	redantimp' p (Disj ds) b l g = do
	    vs <- mapM (const (newSym "d")) ds
	    gp <- redant0 (zipWith (\ v (_, d) -> A (Var v) (d :-> b)) vs ds ++ l) g
	    foldM (\ r (i, v, (cd, _)) -> subst (inj cd i p) v r) gp (zip3 [0..] vs ds)
	-- p : (c -> d) -> b
	redantimp' p (c :-> d) b l g = redantimpimp p c d b l g

	redantimpimp :: Term -> Formula -> Formula -> Formula -> Antecedents -> Goal -> P Proof
	redantimpimp f b c d a g =
	    wrapM "redantimpimp" (b,c,d,a,g) $
	    redantimpimp' f b c d a g

	-- Reduce a double implication antecedent
	redantimpimp' :: Term -> Formula -> Formula -> Formula -> Antecedents -> Goal -> P Proof
	-- next clause exploits ~(C->D) <=> (~~C & ~D)
	-- which isn't helpful when D = false
	redantimpimp' p c d (Disj []) l g | d /= false = do
	    x <- newSym "x"
	    y <- newSym "y"
	    gp <- redantimpimp (Var x) c false false (A (Var y) (d :-> false) : l) g
	    cImpDImpFalse x y p gp
	-- p : (c -> d) -> b
	redantimpimp' p c d b l g = redant more l atomImps (addNestImp (NestImp p c d b) nestImps) atoms g

	-- Reduce an atomic implication
	redantimpatom :: Term -> Symbol -> Formula -> Antecedents -> Goal -> P Proof
	redantimpatom p s b l g =
	    wrapM "redantimpatom" (s,b,l,g) $
	    redantimpatom' p s b l g

	redantimpatom' :: Term -> Symbol -> Formula -> Antecedents -> Goal -> P Proof
	redantimpatom' p s b l g = 
	  do
            a <- cutSearch more $ many (findAtoms s atoms)
            x <- newSym "x"
            gp <- redant1 (A (Var x) b) l g
            mtrace "redantimpatom: LLL" $
             subst (applyAtom p a) x gp
          `mplus`
            (mtrace "redantimpatom: RRR" $
             redant more l (insert atomImps (AtomImp s [A p b])) nestImps atoms g)
{-
	    let ps = wrap "redantimpatom findAtoms" atoms $ findAtoms s atoms
	    in  if not (null ps) then do
		    a <- cutSearch more $ many ps
		    x <- newSym "x"
		    gp <- redant1 (A (Var x) b) l g
		    mtrace "redantimpatom: LLL" $
		     subst (applyAtom p a) x gp
		else
		    mtrace "redantimpatom: RRR" $
		     redant more l (insert atomImps (AtomImp s [A p b])) nestImps atoms g
-}
	-- Reduce the goal, with all antecedents already being classified
	redsucc :: Goal -> P Proof
	redsucc g =
	    wrapM "redsucc" (g, atomImps, nestImps, atoms) $
	    redsucc' g

	redsucc' :: Goal -> P Proof
	redsucc' a@(PVar s) =
	    (cutSearch more $ many (findAtoms s atoms))
	  `mplus`
	    -- The posin check is an optimization.  It gets a little slower without the test.
	    (if posin s atomImps nestImps then
	        redsucc_choice a
	    else
		none)
	redsucc' (Conj cs) = do
	    ps <- mapM redsucc cs
	    return $ applys (Ctuple (length cs)) ps
	-- next clause deals with succedent (A v B) by pushing the
	-- non-determinism into the treatment of implication on the left
	redsucc' (Disj ds) = do
	    s1 <- newSym "_"
	    let v = PVar s1
	    redant0 [ A (Cinj cd i) $ d :-> v | (i, (cd, d)) <- zip [0..] ds ] v
	redsucc' (a :-> b) = do
	    s <- newSym "x"
	    p <- redant1 (A (Var s) a) [] b
	    return $ Lam s p

	-- Now we have the hard part; maybe lots of formulae 
	-- of form (C->D)->B  in nestImps to choose from!
	-- Which one to take first? We user the order heuristic.
	redsucc_choice :: Goal -> P Proof
	redsucc_choice g =
	    wrapM "redsucc_choice" g $
	    redsucc_choice' g

	redsucc_choice' :: Goal -> P Proof
	redsucc_choice' g = do
	    let ordImps = order nestImps g atomImps
	    (NestImp p c d b, restImps) <- 
		mtrace ("redsucc_choice: order=" ++ show ordImps) $
		select ordImps
	    x <- newSym "x"
	    z <- newSym "z"
	    qz <- redant more [A (Var z) $ d :-> b] atomImps restImps atoms (c :-> d)
	    gp <- redant more [A (Var x) b] atomImps restImps atoms g
	    subst (applyImp p (Lam z qz)) x gp

posin :: Symbol -> AtomImps -> NestImps -> Bool
posin g atomImps nestImps = posin1 g atomImps || posin2 g [ (a :-> b) :-> c | NestImp _ a b c <- nestImps ]

posin1 :: Symbol -> AtomImps -> Bool
posin1 g atomImps = any (\ (AtomImp _ bs) -> posin2 g [ b | A _ b <- bs]) atomImps

posin2 :: Symbol -> [Formula] -> Bool
posin2 g bs = any (posin3 g) bs

posin3 :: Symbol -> Formula -> Bool
posin3 g (Disj as) = all (posin3 g) (map snd as)
posin3 g (Conj as) = any (posin3 g) as
posin3 g (_ :-> b) = posin3 g b
posin3 s (PVar s') = s == s'

cutSearch :: MoreSolutions -> P a -> P a
cutSearch False p = atMostOne p
cutSearch True p = p

---------------------------
