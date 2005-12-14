module HCheck(htCheck) where
import List(union)
--import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Data.IntMap(IntMap, insert, (!), empty)

import Util.Digraph(stronglyConnComp, SCC(..))

import HTypes

--import Debug.Trace

type KState = (Int, IntMap (Maybe Kind))
initState :: KState
initState = (0, empty)

type M a = StateT KState (Either String) a

type KEnv = [(HSymbol, Kind)]

data Kind
    = KStar
    | KArrow Kind Kind
    | KVar Int

newKVar :: M Kind
newKVar = do
    (i, m) <- get
    put (i+1, insert i Nothing m)
    return $ KVar i

getVar :: Int -> M (Maybe Kind)
getVar i = do
    (_, m) <- get
    case m!i of
	Just (KVar i') -> getVar i'
	mk -> return mk

addMap :: Int -> Kind -> M ()
addMap i k = do
    (n, m) <- get
    put (n, insert i (Just k) m)

clearState :: M ()
clearState = put initState

htCheck :: [(HSymbol, ([HSymbol], HType))] -> Either String ()
htCheck its =
    let graph = [ (n, i, getHTCons t) | n@(i, (_, t)) <- its ]
	order = stronglyConnComp graph
    in  case [ c | CyclicSCC c <- order ] of
	c : _ -> Left $ "Recursive types are not allowed: " ++ unwords [ i | (i, (_, _)) <- c ]
	[] -> flip evalStateT initState $ inferKinds [] $ map (\ (AcyclicSCC n) -> n) order

inferKinds :: KEnv -> [(HSymbol, ([HSymbol], HType))] -> M ()
inferKinds _env [] = return ()
inferKinds env ((i, (vs, t)) : its) = do
    k <- inferKind env vs t
    inferKinds ((i, k) : env) its

inferKind :: KEnv -> [HSymbol] -> HType -> M Kind
inferKind env vs t = do
    clearState
    ks <- mapM (const newKVar) vs
    let env' = zip vs ks ++ env
    k <- iKind env' t
    ground $ foldr KArrow k ks

iKind :: KEnv -> HType -> M Kind
iKind env (HTApp f a) = do
    kf <- iKind env f
    ka <- iKind env a
    r <- newKVar
    unifyK (KArrow ka r) kf
    return r
iKind env (HTVar v) = do
    getVarKind env v
iKind env (HTCon c) = do
    getConKind env c
iKind env (HTTuple ts) = do
    mapM_ (iKindStar env) ts
    return KStar
iKind env (HTArrow f a) = do
    iKindStar env f
    iKindStar env a
    return KStar
iKind env (HTUnion cs) = do
    mapM_ (\ (_, ts) -> mapM_ (iKindStar env) ts) cs
    return KStar

iKindStar :: KEnv -> HType -> M ()
iKindStar env t = do
    k <- iKind env t
    unifyK k KStar

unifyK :: Kind -> Kind -> M ()
unifyK k1 k2 = do
    let follow k@(KVar i) = getVar i >>= return . maybe k id 
	follow k = return k
	unify KStar KStar = return ()
	unify (KArrow k11 k12) (KArrow k21 k22) = do unifyK k11 k21; unifyK k12 k22
	unify (KVar i1) (KVar i2) | i1 == i2 = return ()
	unify (KVar i) k = do occurs i k; addMap i k
	unify k (KVar i) = do occurs i k; addMap i k
	unify _ _ = lift $ Left "kind error"
	occurs _ KStar = return ()
	occurs i (KArrow f a) = do follow f >>= occurs i; follow a >>= occurs i
	occurs i (KVar i') = if i == i' then lift $ Left "cyclic kind" else return ()
    k1' <- follow k1
    k2' <- follow k2
    unify k1' k2'
    

getVarKind :: KEnv -> HSymbol -> M Kind
getVarKind env v =
    case lookup v env of
    Just k -> return k
    Nothing -> lift $ Left $ "type variable not bound " ++ v

getConKind :: KEnv -> HSymbol -> M Kind
getConKind env v =
    case lookup v env of
    Just k -> return k
    Nothing -> newKVar		-- allow uninterpreted type constructors

ground :: Kind -> M Kind
ground KStar = return KStar
ground (KArrow k1 k2) = liftM2 KArrow (ground k1) (ground k2)
ground (KVar i) = do
    mk <- getVar i
    case mk of
	Just k -> return k
	Nothing -> return KStar

getHTCons :: HType -> [HSymbol]
getHTCons (HTApp f a) = getHTCons f `union` getHTCons a
getHTCons (HTVar _) = []
getHTCons (HTCon s) = [s]
getHTCons (HTTuple ts) = foldr union [] (map getHTCons ts)
getHTCons (HTArrow f a) = getHTCons f `union` getHTCons a
getHTCons (HTUnion alts) = foldr union [] [ getHTCons t | (_, ts) <- alts, t <- ts ]
