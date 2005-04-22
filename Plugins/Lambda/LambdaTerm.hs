
module Plugins.Lambda.LambdaTerm where

import Plugins.Lambda.LangPack

import Map (Map)
import qualified Map as Map hiding (Map)

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont

import Data.Dynamic

type Fun m = (m Dynamic -> m Dynamic)

data LambdaTerm term
   = Lam !String term
   | App term term
   | Var !String

type IState m = (Int,Map Int (m Dynamic),Int) -- record instead of tuple would be more robust
type Ref = Int

prjF :: (Typeable (m Dynamic), MonadError String m) => m Dynamic -> m (Fun m)
prjF = prj'

phiLambda :: (MonadError String m,
              MonadState (IState m) m,
              Typeable (m Dynamic),
              Pause m (Either Dynamic b),
              Typeable b,
              MonadReader (Map String (m Dynamic)) m) =>
              LambdaTerm (m Dynamic) -> m Dynamic
phiLambda (Var v) = do step; lookupEnv v
{-
phiLambda (Lam v b) = do env <- ask
                         return $ inj (\v' -> do loc <- newRef undefined
                                                 let thunk =
                                                       do r <- v'
                                                          writeRef loc
                                                                   (return r)
                                                          return r
                                                 writeRef loc thunk
                                                 let env' = addToFM env v (readRef loc)
                                                 inEnv env' b)
-}
phiLambda (Lam v b) = do env <- ask
                         return $ inj (\v' -> do rr <- thunkify v'
                                                 inEnv (Map.insert v rr env) b)
phiLambda (App f x) = do f' <- prjF f; env <- ask; f' (inEnv env x)

thunkify :: (MonadState (IState m) m) => m Dynamic -> m (m Dynamic)
thunkify c = do
    loc <- newRef (error "empty reference")
    let thunk = do r <- c
                   writeRef loc (return r)
                   return r
    writeRef loc thunk
    return (readRef loc)

lookupEnv :: (MonadReader (Map String (m Dynamic)) m,
              Typeable (m Dynamic),
              MonadError String m) => String -> m Dynamic
lookupEnv k = do env <- ask
                 let v = Map.lookup k env 
                 maybe (throwError ("unbound variable: "++k)) id v

inEnv :: (MonadReader (Map String (m Dynamic)) m) =>
      Map String (m Dynamic) -> m Dynamic -> m Dynamic
inEnv env b = local (const env) b

step :: (MonadState (IState m) m, MonadError String m,
        Typeable b,
        Pause m (Either Dynamic b)) => m ()
step = do (c,hp,fuel) <- get
          if fuel == 0 then do refuel <- pause $ Left . inj
                               put (c,hp,refuel)
                       else put (c,hp,fuel-1)

newRef :: (MonadState (IState m) m) => m Dynamic -> m Ref
newRef v = do (c,hp,fuel) <- get
              put (c+1,Map.insert c v hp,fuel)
              return c

readRef :: (MonadState (IState m) m) => Ref -> m Dynamic
readRef loc = do (_,hp,_) <- get; let {Just x = Map.lookup loc hp}; x

writeRef :: (MonadState (IState m) m) => Ref -> m Dynamic -> m ()
writeRef loc v = do (c,hp,fuel) <- get
                    put (c,Map.insert loc v hp,fuel)

instance Functor LambdaTerm where
    fmap _ (Var v) = Var v
    fmap f (Lam v b) = Lam v (f b)
    fmap f (App g x) = App (f g) (f x)
