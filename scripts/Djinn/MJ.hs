module MJ(module LJTFormula, provable, prove, buildGraph) where
--import Monad
import Control.Monad.State
import Data.List((\\), partition, nubBy, sort)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map(Map, (!), empty, insert, member, assocs, filterWithKey)
import Data.Map(toList, size)
import Data.Queue as Q

import Util(mapFst)
import MonadBFS
import Poly
import LJTFormula

import Debug.Trace
mtrace :: String -> a -> a
mtrace a x = if debug then trace ("*** " ++ a) x else x
debug :: Bool
debug = False

------------------------------
----- Our Proof monad, P, a monad with state and multiple results

type P a = StateT Integer BFS a

nextInt :: P Integer
nextInt = do
   i <- get
   put (i+1)
   return i

none :: P a
none = lift mzero

many :: [a] -> P a
many xs = lift $ msum $ map return xs

wrap :: P a -> P a
wrap (StateT f) = StateT $ \ s -> mwrap (f s)


runP :: P a -> [a]
runP pa = runBFS $ evalStateT pa 0

{-
--runForest :: Forest a -> [a]
runForest' fr = run [fr]
  where run [] = []
        run (Forest xs : q) =
            mtrace ("Q " ++ show q) $
            [x | Tip x <- xs] ++ 
            run (q ++ [ f | Fork f <- xs])
-}
------------------------------
----- Generate a new unique variable
newSym :: String -> P Symbol
newSym pre = do
   i <- nextInt
   return $ Symbol $ pre ++ show i

------------------------------

provable :: Formula -> Bool
provable = not . null . prove False []
prove :: Bool -> [(Symbol, Formula)] -> Formula -> [Term]
prove more as f = runP $ proveP more as f

data M
    = Semi V Ms
    | VLam V M
    | In ConsDesc Int M
    | Tuple [M]
--    | WLam W M
--    | PairQ T M
    deriving (Show)

infixr 5 :::
data Ms
    = Nil
    | M ::: Ms
    | When [ConsDesc] [(V,M)]
    | Sel Int Int Ms
--    | Apq T Ms
--    | Spl W V M
    deriving (Show)

data V = V Symbol
    deriving (Show)

--data W = W Symbol
--    deriving (Show)

--data T = T
--    deriving (Show)

------------------------------

proveP :: Bool -> [(Symbol, Formula)] -> Formula -> P Term
proveP _ env f = do
    let s = initialSequent (map snd env) f
        g = buildGraph s
        vcs = countSequents g s
        g' = pruneGraph g vcs
    () <- mtrace (unlines ("---------" : show f : show (size g) : map show (toList g))) $ return ()
--    () <- trace (unlines (show f : show (size g) : map show vcs)) $ return ()
    () <- mtrace (unlines ("---------" : show f : show (lookup (SVar s) vcs) : show (size g') : map show (toList g'))) $ return ()
    if size g' == 0 then
        none
     else do
        m <- unfold (recGraph s g') (mapFst V env)
        let t = theta m
        insertSplit t

collectSels :: Term -> [Term]
collectSels (Lam _ t) = collectSels t
collectSels (Apply f a) = collectSels f ++ collectSels a
collectSels e@(Xsel _ _ b) = collectSels b ++ [e]
collectSels _ = []

insertSplit :: Term -> P Term
insertSplit t = do
    let sels = nubBy (\ (Xsel _ _ e) (Xsel _ _ e') -> e == e') $ collectSels t
        selTbl = [ (e, n, freeVars e) | Xsel _ n e <- sels ]
        ins :: [(Term, Int, [Symbol])] -> Map Term [Term] -> Term -> P Term
        ins  tbl  stbl (Lam s b) = do
                let tbl' = [ (e, n, fv \\ [s]) | (e, n, fv) <- tbl ]
                    (spls, tbl'') = partition (\ (_,_,fv) -> null fv) tbl'
                    mkSplit (f, sps) (e, n, _) = do
                        vars <- mapM (const (newSym "s")) [1..n]
                        let sps' = insert e (map Var vars) sps
                        e' <- ins tbl'' sps' e
                        let fun oe = Apply (Apply (Csplit n) (foldr Lam oe vars)) e'
                        return (f . fun, sps')
                (trfun, stbl') <- foldM mkSplit (id, stbl) spls
                e' <- ins tbl'' stbl' b
                return $ Lam s (trfun e')
        ins  tbl  stbl (Apply f a) = liftM2 Apply (ins tbl stbl f) (ins tbl stbl a)
        ins _tbl  stbl (Xsel i _ e) = --trace ("Xsel " ++ show (_tbl, stbl, i, e)) $
                return $ (stbl ! e) !! i
        ins _tbl _stbl e = return e
--    () <- trace ("insertSplit " ++ show (t, selTbl)) $ return ()
    t' <- ins selTbl empty t
    if t == t' then
        return t
     else
--      trace ("insertSplit recurses") $
        insertSplit t'


------------------------------

theta :: M -> Term
theta (Semi (V s) ms) = theta' (Var s) ms
theta (VLam (V s) m) = Lam s (theta m)
theta (In cd i m) = Apply (Cinj cd i) (theta m)
theta (Tuple ms) = foldl Apply (Ctuple (length ms)) (map theta ms)

theta' :: Term -> Ms -> Term
theta' a Nil = a
theta' a (m ::: ms) = theta' (Apply a (theta m)) ms
theta' a (When cds vms) = foldl Apply (Ccases cds) (a : [ Lam s $ theta m | (V s, m) <- vms ])
theta' a (Sel i n ms) = theta' (Xsel i n a) ms
--  where sel = Apply (Csplit n) (foldr Lam (Var (xs!!i)) xs) where xs = [ Symbol ("_x" ++ show j) | j <- [0..n-1] ]

------------------------------

type Context = [(V, Formula)]

addCtx :: V -> Formula -> Context -> Context
addCtx v f ctx = (v, f) : ctx


data Gamma = Gamma (S.Set Formula)
    deriving (Show, Eq, Ord)

addEnv :: Formula -> Gamma -> Gamma
addEnv f (Gamma fs) = Gamma $ S.insert f fs

envList :: Gamma -> [Formula]
envList (Gamma fs) = S.elems fs

data Sequent = S Gamma (Maybe Formula) Formula
    deriving (Show, Eq, Ord)

initialSequent :: [Formula] -> Formula -> Sequent
initialSequent g f = S (Gamma $ S.fromList g) Nothing f

data Rule = OrL [(ConsDesc, Formula)] | OrR ConsDesc Int | AndL Int Int 
          | AndR | ImpL | ImpR Formula | Ax | Cont Formula
    deriving (Show, Eq, Ord)

data VP a = VP Rule [a]
    deriving (Show, Eq, Ord)

type Graph = Map Sequent [VP Sequent]

type GraphRec = Next
data Next = Next { unNext :: [VP Next] }

buildG :: Queue Sequent -> Graph -> Graph
buildG q g =
    case deQueue q of
    Nothing -> g
    Just (sq, q') ->
        let vps = getVPs sq ++ getVpCont sq
            g' = insert sq vps g
            (q'', g'') = foldr addSeq (q', g') [ s | VP _ ss <- vps, s <- ss ]
        in  buildG q'' g''
  where addSeq s o@(oq, og) = if member s og then o else (addToQueue oq s, insert s [] og)

        getVPs (S env (Just (Disj ds)) c) =
            [ VP (OrL ds) [ S (addEnv a env) Nothing c | (_, a) <- ds ] ]
        getVPs (S env (Just (Conj as)) b) =
            [ VP (AndL i (length as)) [S env (Just a) b] | (a, i) <- zip as [0..] ]
        getVPs (S env (Just (a :-> b)) c) =
            [ VP ImpL [S env Nothing a, S env (Just b) c] ]
        getVPs (S _env (Just x) x') | x == x' =
            [ VP Ax [] ]

        getVPs (S env Nothing (Disj ds)) =
            [ VP (OrR (fst (ds!!i)) i) [S env Nothing a] | ((_,a), i) <- zip ds [0..] ]
        getVPs (S env Nothing (Conj as)) =
            [ VP AndR [ S env Nothing a | a <- as ] ]
        getVPs (S env Nothing (a :-> b)) =
            [ VP (ImpR a) [ S (addEnv a env) Nothing b ] ]

        getVPs (S _ _ _) =
            [ ]

        getVpCont (S env Nothing b) =
            [ VP (Cont a) [S env (Just a) b] | a <- envList env ]
        getVpCont (S _ _ _) =
            [ ]
                

buildGraph :: Sequent -> Graph
buildGraph s = buildG (addToQueue emptyQueue s) empty


data SVar = SVar Sequent
    deriving (Eq, Ord, Show)

newtype TV = TV Int
    deriving (Eq, Ord)
instance Show TV where
    show (TV i) = "x" ++ show i

countSequents :: Graph -> Sequent -> [(SVar, Ninf)]
--countSequents g s = solveEqnSystem $ buildEqns g (const Nothing) s
countSequents g s =
    let eqns = buildEqns g (const Nothing) s
        subst = [(v, TV i) | ((v, _), i) <- zip eqns [0..]] 
        eqns' = zip (map snd subst) (map (substPolyVars subst . snd) eqns)
        sol = solveEqnSystem eqns'
    in  zip (map fst eqns) (map snd (sort sol))

buildEqns :: Graph -> (Sequent -> Maybe Ninf) -> Sequent -> EqnSystem SVar
buildEqns graph oracle seqnt = assocs $ build seqnt empty
  where build :: Sequent -> Map SVar (Poly SVar) -> Map SVar (Poly SVar)
        build s r =
            let sv = SVar s in
            if sv `member` r then
                r
            else case oracle s of
                 Just n -> insert sv (constp n) r
                 Nothing ->
                    let v = sum [ product [ var (SVar n) | n <- ns ] | VP _ ns <- graph!s ]
                        r' = insert sv v r
                        l = [ n | VP _ ns <- graph!s, n <- ns ]
                    in  foldr build r' l

pruneGraph :: Graph -> [(SVar, Ninf)] -> Graph
pruneGraph g vcs =
    let zset = S.fromList [ s | (SVar s, 0) <- vcs ]
        g' = filterWithKey (\ k _ -> not (S.member k zset)) g
        g'' = M.map (\ vps -> filter (\ (VP _ ns) -> not (any (`S.member` zset) ns)) vps) g'
    in  g''

recGraph :: Sequent -> Graph -> GraphRec
recGraph s g =
    let m :: M.Map Sequent GraphRec
        m = M.map (\ vps -> Next $ map (\ (VP r ss) -> VP r (map (m M.!) ss)) vps) g
    in  m M.! s

------------------------------

unfold :: GraphRec -> Context -> P M
unfold graph context = unfoldM context graph
  where unfoldM  ctx s = wrap $ msum $ map (unfoldVPM  ctx) (unNext s)
        unfoldMs ctx s = wrap $ msum $ map (unfoldVPMs ctx) (unNext s)

        unfoldVPMs ctx (VP (OrL cfs) ns) = do
            vms <- zipWithM (\ n (_, a) -> do
                     x <- liftM V $ newSym "c"
                     m <- unfoldM (addCtx x a ctx) n
                     return (x, m)
                ) ns cfs
            return $ When (map fst cfs) vms
        unfoldVPMs ctx (VP (AndL i n) [s]) = do
            m <- unfoldMs ctx s
            return $ Sel i n m
        unfoldVPMs ctx (VP ImpL [sa, sc]) = do
            u <- unfoldM ctx sa
            l <- unfoldMs ctx sc
            return $ u ::: l
        unfoldVPMs _ctx (VP Ax []) =
            return Nil
        unfoldVPMs _ctx _vp = error $ "unfoldVPMs " -- ++ show vp

        unfoldVPM ctx (VP (OrR c i) [s]) = do
            u <- unfoldM ctx s
            return $ In c i u
        unfoldVPM ctx (VP AndR ns) = do
            ts <- mapM (unfoldM ctx) ns
            return $ Tuple ts
        unfoldVPM ctx (VP (ImpR a) [s]) = do
            x <- liftM V $ newSym "i"
            u <- unfoldM (addCtx x a ctx) s
            return $ VLam x u
        unfoldVPM ctx (VP (Cont ca) [s]) = do
            (x, _a) <- many $ filter ((== ca) . snd) ctx
            l <- unfoldMs ctx s
            return $ Semi x l
        
        unfoldVPM _ctx _vp = error $ "unfoldVPM " -- ++ show vp
