{-# OPTIONS -w #-}

module Plugin.Free.FreeTheorem where

import Plugin.Free.Type
import Plugin.Free.Expr
import Plugin.Free.Theorem
import Plugin.Free.Parse
import Plugin.Free.Util

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

import Data.Char
import qualified Data.Map as M

newtype MyState
    = MyState {
        myVSupply :: Int
    }

type MyMon a = StateT MyState Identity a

type TyEnv = [(TyVar,Var,TyVar,TyVar)]

makeVar :: String -> MyMon String
makeVar v
    = do
        vn <- gets myVSupply
        modify (\s -> s { myVSupply = vn+1 })
        return (v ++ "_" ++ show vn)

extractTypes :: TyEnv -> Type -> (Type,Type)
extractTypes env (TyVar v)
    = head [ (TyVar t1,TyVar t2) | (v',_,t1,t2) <- env, v == v' ]
extractTypes env (TyForall v t)
    = let (t1,t2) = extractTypes ((v,undefined,v,v):env) t
      in (TyForall v t1, TyForall v t2)
extractTypes env (TyArr t1 t2)
    = let (t1a,t1b) = extractTypes env t1
          (t2a,t2b) = extractTypes env t2
      in (TyArr t1a t2a, TyArr t1b t2b)
extractTypes env (TyTuple ts)
    = let ts12 = map (extractTypes env) ts
      in (TyTuple (map fst ts12), TyTuple (map snd ts12))
extractTypes env (TyCons c ts)
    = let ts12 = map (extractTypes env) ts
      in (TyCons c (map fst ts12), TyCons c (map snd ts12))

freeTheoremStr :: (Monad m) => (String -> m String) -> String -> m String
freeTheoremStr tf s
    = case parse (do
                    Just (QVarId v) <- getToken
                    (mplus (do match OpColonColon
                               t <- parseType
                               return $ Left (v,t))
                           (return (Right v)))) (lexer s) of
        ParseSuccess (Left (v,t)) [] -> return (run' v t)
        ParseSuccess (Right v)    [] -> do tStr <- tf s
                                           case parse parseType (lexer tStr) of
                                             ParseSuccess t [] -> return (run' v t)
                                             ParseSuccess _ _ -> return $ "Extra stuff at end of line in retrieved type " ++ show tStr
                                             ParseError msg -> return msg
        ParseSuccess _ _      -> return "Extra stuff at end of line"
        ParseError msg        -> return msg
    where
        run' v t = renderStyle defstyle (pretty (freeTheorem v t))
        defstyle = Style {
                        mode = PageMode,
                        lineLength = 78,
                        ribbonsPerLine = 1.5
                    }

freeTheorem :: String -> Type -> Theorem
freeTheorem name t
    = runIdentity $ do
        (th,_) <- runStateT (freeTheorem' [] v0 v0 t) initState
        let th' = theoremSimplify th
        return . fst $ runState (insertRn name name >> rename th') initRnSt
    where
        v0 = EVar name
        initState   = MyState { myVSupply = 1 }

------------------------------------------------------------------------
-- Rename monad, and pretty alpha renamer

data RnSt = RnSt { gamma  :: M.Map Var Var
                 , unique   :: [Var]
                 , uniquelist :: [Var]
                 , uniquefn :: [Var]
                 }
    deriving Show

initRnSt
    = RnSt M.empty suggestionsVal suggestionsList suggestionsFun
    where
        suggestionsVal = map (:[]) "xyzuvabcstdeilmnorw"
                            ++ [ 'x' : show i | i <- [1..] ]
        suggestionsList = map (:"s") "xyzuvabcstdeilmnorw"
                            ++ [ "xs" ++ show i | i <- [1..] ]
        suggestionsFun = map (:[]) "fghkpq"
                            ++ [ 'f' : show i | i <- [1..] ]

type RN a = State RnSt a

-- generate a nice fresh name
freshName :: RN Var
freshName = do
    s <- get
    let ns    = unique s
        fresh = head ns
    put $ s { unique = tail ns }
    case M.lookup fresh (gamma s) of
        Nothing -> return fresh
        _       -> freshName

-- generate a nice function name
freshFunctionName :: RN Var
freshFunctionName = do
    s <- get
    let ns    = uniquefn s
        fresh = head ns
    put $ s { uniquefn = tail ns }
    case M.lookup fresh (gamma s) of
        Nothing -> return fresh
        _       -> freshFunctionName

-- generate a nice list name
freshListName :: RN Var
freshListName = do
    s <- get
    let ns    = uniquelist s
        fresh = head ns
    put $ s { uniquelist = tail ns }
    case M.lookup fresh (gamma s) of
        Nothing -> return fresh
        _       -> freshListName

-- insert a new association into the heap
insertRn :: Var -> Var -> RN ()
insertRn old new = modify $ \s ->
    let gamma' = M.insert old new (gamma s) in s { gamma = gamma' }

-- lookup the binding
lookupRn :: Var -> RN Var
lookupRn old = do
    m <- gets gamma
    return $ case M.lookup old m of
        Nothing  -> old
        Just new -> new

-- alpha rename a simplified theory to something nice
rename :: Theorem -> RN Theorem
rename (ThImplies th1 th2) = do
    th1' <- rename th1
    th2' <- rename th2
    return $ ThImplies th1' th2'

rename (ThEqual e1 e2) = do
    e1' <- rnExp e1
    e2' <- rnExp e2
    return $ ThEqual e1' e2'

rename (ThAnd th1 th2) = do
    th1' <- rename th1
    th2' <- rename th2
    return $ ThAnd th1' th2'

rename (ThForall v ty th) = do
    v' <- case ty of
                TyArr _ _     -> freshFunctionName
                TyCons "[]" _ -> freshListName
                _             -> freshName
    insertRn v v'
    ty' <- rnTy ty
    th' <- rename th
    return $ ThForall v' ty' th'

rnExp :: Expr -> RN Expr
rnExp e@(EBuiltin _) = return e
rnExp (EVar v)       = EVar       `fmap` lookupRn v
rnExp (EVarOp f n v) = EVarOp f n `fmap` lookupRn v

rnExp (EApp e1 e2) = do
    e1' <- rnExp e1
    e2' <- rnExp e2
    return (EApp e1' e2')

rnExp (ETyApp e ty) = do
    e'  <- rnExp e
    ty' <- rnTy ty
    return (ETyApp e' ty')

rnTy :: Type -> RN Type
rnTy ty = return ty

------------------------------------------------------------------------

freeTheorem' :: TyEnv -> Expr -> Expr -> Type -> MyMon Theorem

freeTheorem' env e1 e2 t'@(TyForall v t)
    = do
        mv <- makeVar "f"
        t1 <- makeVar v
        t2 <- makeVar v
        let tymv = TyArr (TyVar t1) (TyVar t2)
        pt <- freeTheorem' ((v,mv,t1,t2):env) (ETyApp e1 (TyVar t1))
                                              (ETyApp e2 (TyVar t2)) t
        return (ThForall mv tymv pt)

freeTheorem' env e1 e2 t'@(TyArr t1 t2)
    = do
        mv1 <- makeVar "v1"
        mv2 <- makeVar "v2"
        let (tmv1,tmv2) = extractTypes env t1
        p1 <- freeTheorem' env (EVar mv1) (EVar mv2) t1
        p2 <- freeTheorem' env (EApp e1 (EVar mv1)) (EApp e2 (EVar mv2)) t2
        return (ThForall mv1 tmv1 (ThForall mv2 tmv2 (ThImplies p1 p2)))

freeTheorem' env e1 e2 t'@(TyTuple [])
    = do
        return (ThEqual e1 e2)

freeTheorem' env e1 e2 t'@(TyTuple ts)
    = do
        let len = length ts

        fts <- mapM (\t -> do
                let (t1,t2) = extractTypes env t
                f <- makeVar "f"
                x <- makeVar "x"
                y <- makeVar "y"
                th <- freeTheorem' env (EVar x) (EVar y) t
                let eq = ThEqual (EApp (EVar f) (EVar x)) (EVar y)
                return ((f,TyArr t1 t2),
                        ThForall x t1 (
                            ThForall y t2 (
                                ThImplies th eq
                            )
                        )
                    )
            ) ts
        let thf = ThEqual (EApp (foldl (\e ((f,_),_) -> EApp e (EVar f))
                            (EBuiltin $ BMapTuple len) fts) e1) e2
        return (foldr (\((f,t),e1) e2 -> ThForall f t (ThImplies e1 e2))
                thf fts)

freeTheorem' env e1 e2 t'@(TyVar v)
    = do
        let f = head [ f | (v',f,_,_) <- env, v' == v ]
        return (ThEqual (EApp (EVar f) e1) e2)

freeTheorem' env e1 e2 t'@(TyCons _ [])
    = do
        return (ThEqual e1 e2)

freeTheorem' env e1 e2 t'@(TyCons c [t])
    = do
        f <- makeVar "f"
        x <- makeVar "x"
        y <- makeVar "y"
        let (t1,t2) = extractTypes env t
        p1 <- freeTheorem' env (EVar x) (EVar y) t
        let p2 = ThEqual (EApp (EVar f) (EVar x)) (EVar y)
        let p3 = ThEqual (EApp (EApp (EBuiltin (BMap c)) (EVar f)) e1) e2
        return (ThForall f (TyArr t1 t2) (
                ThImplies (ThForall x t1 (ThForall y t2 (ThImplies p1 p2)))
                            p3))

freeTheorem' env e1 e2 t'@(TyCons c@"Either" ts@[_,_])
    = do
        fts <- mapM (\t -> do
                let (t1,t2) = extractTypes env t
                f <- makeVar "f"
                x <- makeVar "x"
                y <- makeVar "y"
                th <- freeTheorem' env (EVar x) (EVar y) t
                let eq = ThEqual (EApp (EVar f) (EVar x)) (EVar y)
                return ((f,TyArr t1 t2),
                        ThForall x t1 (
                            ThForall y t2 (
                                ThImplies th eq
                            )
                        )
                    )
            ) ts
        let thf = ThEqual (EApp (foldl (\e ((f,_),_) -> EApp e (EVar f))
                            (EBuiltin $ BMap c) fts) e1) e2
        return (foldr (\((f,t),e1) e2 -> ThForall f t (ThImplies e1 e2))
                thf fts)

-- vim: ts=4:sts=4:expandtab:ai
