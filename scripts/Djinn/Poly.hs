module Poly(Ninf, inf, ninfToN,
            Poly, var, constp, coeffsOf, mustBeConst, substPolyVars,
            EqnSystem, solveEqnSystem) where
import List(partition, sort)
import Util.Digraph(stronglyConnComp, SCC(..))
import Util(mapSnd, insert, makeSet)

---------------------------------------
----- The type naturals + infinity

data Ninf = I !Integer | Inf

inf :: Ninf
inf = Inf

ninfToN :: Ninf -> Maybe Integer
ninfToN Inf = Nothing
ninfToN (I i) = Just i

instance Show Ninf where
    showsPrec p (I i) = showsPrec p i
    showsPrec _ Inf = showString "inf"

instance Eq Ninf where
    I i == I j  =  i == j
    Inf == Inf  =  error "Ninf: Inf == Inf" --True       -- XXX?
    _   == _    = False

instance Ord Ninf where
    I i <= I j  =  i <= j
    I _ <= Inf  =  True
    _   <= _    =  False

instance Num Ninf where
    I i + I j  =  I (i + j)
    _   + _    =  Inf
    I i * I j  =  I (i * j)
    I 0 * Inf  =  I 0
    Inf * I 0  =  I 0
    _   * _    =  Inf
    negate _   = error "Ninf: negate"
    abs _   = error "Ninf: abs"
    signum _   = error "Ninf: signum"
    fromInteger i | i < 0 = error "Ninf: fromInteger"
                  | otherwise = I i


---------------------------------------
----- Polynomials over Ninf
-- Parametrized over the variable type

data Poly a = P ![Summand a]            -- summands, sorted
    deriving (Eq, Ord)

data Summand a = S ![(a, Integer)] !Ninf        -- variables with their exponents and scalar factor, sorted
    deriving (Eq, Ord)

instance (Show a) => Show (Poly a) where
    showsPrec _ (P []) = showString "0"
    showsPrec _ (P ss) = mix (map showS ss) " + "
        where mix [] _ = error "Poly.mix"
              mix [x] _ = x
              mix (x:xs) m = x . showString m . mix xs m
              showS (S [] i) = showsPrec 0 i
              showS (S vs 1) = mix (map showV vs) "*"
              showS (S vs n) = mix (showsPrec 0 n : map showV vs) "*"
              showV (v, 1) = showsPrec 0 v
              showV (v, n) = showsPrec 0 v . showString "^" . showsPrec 0 n

mkS :: [(a, Integer)] -> Ninf -> Summand a
mkS ves Inf = S [(v,1) | (v,_) <- ves] Inf
mkS ves k = S ves k

-- Make a polynomial that is just a variable
var :: a -> Poly a
var x = P [S [(x, 1)] 1]

constp :: Ninf -> Poly a
constp 0 = P []
constp i = P [S [] i]

isConstPoly :: Poly a -> Bool
isConstPoly (P []) = True
isConstPoly (P [S [] _]) = True
isConstPoly _ = False

instance (Show a, Ord a) => Num (Poly a) where
    P ss1 + P ss2 = P $ add ss1 ss2
    P ss1 * P ss2 = sum [ P [muls s1 s2] | s1 <- ss1, s2 <- ss2 ]
    negate _   = error "Poly: negate"
    abs _   = error "Poly: abs"
    signum _   = error "Poly: signum"
    fromInteger i | i < 0 = error "Poly: fromInteger"
                  | i == 0 = P []
                  | otherwise = P [S [] (fromInteger i)]

add :: (Ord a) => [Summand a] -> [Summand a] -> [Summand a]
add [] ss = ss
add ss [] = ss
add (s1@(S vs1 k1) : ss1) (s2@(S vs2 k2) : ss2) =
    case compare vs1 vs2 of
    LT -> s1 : add ss1 (s2:ss2)
    EQ -> mkS vs1 (k1 + k2) : add ss1 ss2
    GT -> s2 : add (s1:ss1) ss2

muls :: (Ord a) => Summand a -> Summand a -> Summand a
muls (S vs1 k1) (S vs2 k2) = mkS (merge vs1 vs2) (k1 * k2)
  where merge [] ys = ys
        merge xs [] = xs
        merge (x@(v1, e1):xs) (y@(v2, e2):ys) =
            case compare v1 v2 of
            LT -> x : merge xs (y:ys)
            EQ -> (v1, e1+e2) : merge xs ys
            GT -> y : merge (x:xs) ys

-----
-- Return all the coefficints for a certain variable of
-- a polynomial.  Returns (exponent, coefficient) list, sorted by exponent.

coeffsOf :: (Show a, Ord a) => a -> Poly a -> [(Integer, Poly a)]
coeffsOf x (P ss) = sort $ foldr coeff [] ss
  where coeff s@(S vs k) cs =
            case partition (\ (v, _) -> v == x) vs of
            ([(_, a)], vs') -> addCoeff a (P [S vs' k]) cs
            _ -> addCoeff 0 (P [s]) cs
        addCoeff a c [] = [(a, c)]
        addCoeff a c ((a', c'):cs) | a == a' = (a, c + c') : cs
        addCoeff a c (k:cs) = k : addCoeff a c cs

mustBeConst :: (Show a) => Poly a -> Ninf
mustBeConst (P []) = 0
mustBeConst (P [S [] k]) = k
mustBeConst p = error $ "Poly.mustBeConst: " ++ show p

getPolyVars :: (Ord a) => Poly a -> [a]
getPolyVars (P ss) = makeSet [ x | S vs _ <- ss, (x, _) <- vs ]

substPolyVars :: (Eq a) => [(a, b)] -> Poly a -> Poly b
substPolyVars ons (P ss) = P [ S [ (new v, e) | (v, e) <- ves ] k | S ves k <- ss ]
  where new v = maybe (error "substPolyVars") id $ lookup v ons

---------------------------------------
----- Find the least fixed point solution of an equation system
-- There is always a solution because +, * are monotonic and
-- Ninf is a complete lattice.
-- Uses Zaionc's algorithm from
-- "Fixpoint Technique for Counting Terms in Typed $lambda$-calculus",
-- by Marek Zaionc.

-- The equations are of the form
--   x_1 = poly_1(x_1, ..., x_n)
--   ...
--   x_n = poly_n(x_1, ..., x_n)

type EqnSystem var = [(var, Poly var)]

solveEqnSystem :: (Ord var, Show var) => EqnSystem var -> [(var, Ninf)]
solveEqnSystem eqns =
    -- The definition could be  mapSnd mustBeConst $ solve eqns
    -- but this can be very slow.
    let groups = map flatten $ stronglyConnComp [ (eqn, v, getPolyVars rhs) | eqn@(v, rhs) <- eqns ]
                  where flatten (CyclicSCC es) = es
                        flatten (AcyclicSCC e) = [e]
        solveGroup :: (Ord var, Show var) => [(var, Ninf)] -> EqnSystem var -> [(var, Ninf)]
        solveGroup sols es =
            let newsols = mapSnd mustBeConst $ solve (mapSnd (evalPolyConst sols) es)
            in  foldr insert sols newsols
    in  foldl solveGroup [] groups

solve :: (Ord var, Show var) => EqnSystem var -> [(var, Poly var)]
solve [] = []
solve ((v,f):xfs) =
    let cs = coeffsOf v f
        a0 = getCoeff 0 cs
        a = sum [ ai | (i, ai) <- cs, i /= 0 ]
        g = a0 + a0 * a * constp inf
        xfs' = [ (y, evalPoly [(v, g)] p) | (y, p) <- xfs ]
        sol = solve xfs'
        x = evalPoly sol g
    in  (v, x) : sol

evalPoly :: (Ord var, Show var) => [(var, Poly var)] -> Poly var -> Poly var
evalPoly _ poly | isConstPoly poly = poly -- just a speedup
evalPoly vps poly = foldr eval1 poly vps
  where eval1 (v, p) q = sum [ c * p^e | (e, c) <- coeffsOf v q ]

-- evalPolyConst is just a specialized version of evalConst when
-- the values are constants.  It's also optimized a lot.
evalPolyConst :: (Ord var, Show var) => [(var, Ninf)] -> Poly var -> Poly var
evalPolyConst _ poly | isConstPoly poly = poly -- just a speedup
-- Assume vps is sorted in the same order as vs.
evalPolyConst vcs (P ss) = P (concatMap evalS ss)
  where evalS (S ves k) = loop vcs ves [] k
        loop [] ves rves k = [mkS (reverse rves ++ ves) k]
        loop _  []  rves k = [mkS (reverse rves) k]
        loop ovps@((v,c) : vps) oves@(ve@(v',e) : ves) rves k =
            case v `compare` v' of
            LT -> loop vps oves rves k
            EQ -> if c == 0 then [] else loop vps ves rves (k * c^e)
            GT -> loop ovps ves (ve:rves) k

getCoeff :: (Ord a, Show a) => Integer -> [(Integer, Poly a)] -> Poly a
getCoeff i ips = maybe 0 id $ lookup i ips
