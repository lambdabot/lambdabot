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
extractTypes env (TyList t)
    = let (t1,t2) = extractTypes env t
      in (TyList t1, TyList t2)
extractTypes env (TyTuple ts)
    = let ts12 = map (extractTypes env) ts
      in (TyTuple (map fst ts12), TyTuple (map snd ts12))
extractTypes env (TyCons c ts)
    = let ts12 = map (extractTypes env) ts
      in (TyCons c (map fst ts12), TyCons c (map snd ts12))

freeTheoremStr :: String -> String
freeTheoremStr s
    = case parse (do
                    Just (QVarId v) <- getToken
                    match OpColonColon
                    t <- parseType
                    return (v,t)) (lexer s) of
        ParseSuccess (v,t) [] -> renderStyle defstyle
                                        (pretty (freeTheorem v t))
        ParseSuccess _ _      -> "Extra stuff at end of line"
        ParseError msg        -> msg
    where
        defstyle = Style {
                        mode = PageMode,
                        lineLength = 78,
                        ribbonsPerLine = 1.5
                    }

freeTheorem :: String -> Type -> Theorem
freeTheorem name t
    = runIdentity $ do
        (th,_) <- runStateT (freeTheorem' [] v0 v0 t) initState
        return (theoremSimplify th)
        -- return th
    where
        v0 = EVar name
        initState = MyState { myVSupply = 1 }

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

freeTheorem' env e1 e2 t'@(TyList t)
    = do
        f <- makeVar "f"
        x <- makeVar "x"
        y <- makeVar "y"
        let (t1,t2) = extractTypes env t
        p1 <- freeTheorem' env (EVar x) (EVar y) t
        let p2 = ThEqual (EApp (EVar f) (EVar x)) (EVar y)
        let p3 = ThEqual (EApp (EApp (EVar "$map") (EVar f)) e1) e2
        return (ThForall f (TyArr t1 t2) (
                ThImplies (ThForall x t1 (ThForall y t2 (ThImplies p1 p2)))
                            p3))

freeTheorem' env e1 e2 t'@(TyTuple [])
    = do
        return (ThEqual e1 e2)

freeTheorem' env e1 e2 t'@(TyTuple ts)
    = do
        let len = length ts
        let vcomponents
                = [ EApp (EVar ("$proj_" ++ show len ++ "_" ++ show i))
                                | i <- [1..len] ]
        ps <- mapM (\(vc,t) -> freeTheorem' env (vc e1) (vc e2) t)
                    (zip vcomponents ts)
        return (foldr1 ThAnd ps)

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
        let p3 = ThEqual (EApp (EApp (EVar ("$map_" ++ c)) (EVar f)) e1) e2
        return (ThForall f (TyArr t1 t2) (
                ThImplies (ThForall x t1 (ThForall y t2 (ThImplies p1 p2)))
                            p3))

-- vim: ts=4:sts=4:expandtab:ai
