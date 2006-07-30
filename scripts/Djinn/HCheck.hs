--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module HCheck(htCheckEnv, htCheckType) where
import Data.List(union)
--import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Data.IntMap(IntMap, insert, (!), empty)

import Util.Digraph(stronglyConnComp, SCC(..))

import HTypes

--import Debug.Trace

type KState = (Int, IntMap (Maybe HKind))
initState :: KState
initState = (0, empty)

type M a = StateT KState (Either String) a

type KEnv = [(HSymbol, HKind)]

newKVar :: M HKind
newKVar = do
    (i, m) <- get
    put (i+1, insert i Nothing m)
    return $ KVar i

getVar :: Int -> M (Maybe HKind)
getVar i = do
    (_, m) <- get
    case m!i of
	Just (KVar i') -> getVar i'
	mk -> return mk

addMap :: Int -> HKind -> M ()
addMap i k = do
    (n, m) <- get
    put (n, insert i (Just k) m)

clearState :: M ()
clearState = put initState

htCheckType :: [(HSymbol, ([HSymbol], HType, HKind))] -> HType -> Either String ()
htCheckType its t = flip evalStateT initState $ do
    let vs = getHTVars t
    ks <- mapM (const newKVar) vs
    let env = zip vs ks ++ [(i, k) | (i, (_, _, k)) <- its ]
    iHKindStar env t        

htCheckEnv :: [(HSymbol, ([HSymbol], HType, a))] -> Either String [(HSymbol, ([HSymbol], HType, HKind))]
htCheckEnv its =
    let graph = [ (n, i, getHTCons t) | n@(i, (_, t, _)) <- its ]
	order = stronglyConnComp graph
    in  case [ c | CyclicSCC c <- order ] of
	c : _ -> Left $ "Recursive types are not allowed: " ++ unwords [ i | (i, _) <- c ]
	[] -> flip evalStateT initState $ addKinds
	    where addKinds = do
		        env <- inferHKinds [] $ map (\ (AcyclicSCC n) -> n) order
		  	let getK i = maybe (error $ "htCheck " ++ i) id $ lookup i env
			return [ (i, (vs, t, getK i)) | (i, (vs, t, _)) <- its ]

inferHKinds :: KEnv -> [(HSymbol, ([HSymbol], HType, a))] -> M KEnv
inferHKinds env [] = return env
inferHKinds env ((i, (vs, t, _)) : its) = do
    k <- inferHKind env vs t
    inferHKinds ((i, k) : env) its

inferHKind :: KEnv -> [HSymbol] -> HType -> M HKind
inferHKind env vs t = do
    clearState
    ks <- mapM (const newKVar) vs
    let env' = zip vs ks ++ env
    k <- iHKind env' t
    ground $ foldr KArrow k ks

iHKind :: KEnv -> HType -> M HKind
iHKind env (HTApp f a) = do
    kf <- iHKind env f
    ka <- iHKind env a
    r <- newKVar
    unifyK (KArrow ka r) kf
    return r
iHKind env (HTVar v) = do
    getVarHKind env v
iHKind env (HTCon c) = do
    getConHKind env c
iHKind env (HTTuple ts) = do
    mapM_ (iHKindStar env) ts
    return KStar
iHKind env (HTArrow f a) = do
    iHKindStar env f
    iHKindStar env a
    return KStar
iHKind env (HTUnion cs) = do
    mapM_ (\ (_, ts) -> mapM_ (iHKindStar env) ts) cs
    return KStar

iHKindStar :: KEnv -> HType -> M ()
iHKindStar env t = do
    k <- iHKind env t
    unifyK k KStar

unifyK :: HKind -> HKind -> M ()
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
    

getVarHKind :: KEnv -> HSymbol -> M HKind
getVarHKind env v =
    case lookup v env of
    Just k -> return k
    Nothing -> lift $ Left $ "type variable not bound " ++ v

getConHKind :: KEnv -> HSymbol -> M HKind
getConHKind env v =
    case lookup v env of
    Just k -> return k
    Nothing -> newKVar		-- allow uninterpreted type constructors

ground :: HKind -> M HKind
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
