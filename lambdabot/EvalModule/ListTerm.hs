module EvalModule.ListTerm where

import Control.Monad.Error
import Control.Monad.Reader
import EvalModule.LangPack
import Data.Dynamic
import Data.FiniteMap

import EvalModule.LambdaTerm

data ListTerm term
   = Nil
   | Character !Char
   | Head term
   | Tail term
   | Null term
   | Cons term term
   | Append term term

prjL :: (MonadError String m, 
         Typeable (m Dynamic)) => m Dynamic -> m (m Dynamic, m Dynamic)
prjL = prj'

prjN :: (MonadError String m) => m Dynamic -> m ()
prjN = prj'

prjC :: (MonadError String m) => m Dynamic -> m Char
prjC = prj'

phiList :: (MonadError String m,
            MonadReader (FiniteMap String (m Dynamic)) m,
            Typeable (m Dynamic)) => ListTerm (m Dynamic) -> m Dynamic
phiList Nil = return $ inj ()
phiList (Character c) = return $ inj c
-- TODO: double evaluation of l
phiList (Head l) = do b <- isNull l; if b then throwError "head of empty list"
                                          else prjL l >>= fst
phiList (Tail l) = do b <- isNull l; if b then throwError "tail of empty list"
                                          else prjL l >>= snd
phiList (Null l) = do b <- isNull l; if b then return (inj True)
                                          else do prjL l; return (inj False)
-- TODO: this is call-by-name, not call-by-need
phiList (Cons l r) = do env <- ask; return $ inj (inEnv env l,inEnv env r)
phiList (Append l r) = do env <- ask
                          foldList (\x y -> return $ inj (x,y)) 
                                   (inEnv env r) 
                                   (inEnv env l)

isNull :: (Monad m) => m Dynamic -> m Bool
isNull m = do d <- m; case (fromDynamic d :: Maybe ()) of
                         Just _  -> return True
                         Nothing -> return False

isCharacter :: (Monad m) => m Dynamic -> m Bool
isCharacter m = do d <- m; case (fromDynamic d :: Maybe Char) of
                              Just _  -> return True
                              Nothing -> return False

foldList :: (MonadError String m, Typeable (m Dynamic)) => 
            (m Dynamic -> m a -> m a) -> 
                    m a -> m Dynamic  -> m a
foldList c n l = do
    b <- isNull l
    if b then n
         else do (f,r) <- prjL l
                 c f $ foldList c n r

instance Functor ListTerm where
    fmap f Nil = Nil
    fmap f (Character c) = Character c
    fmap f (Head l) = Head (f l)
    fmap f (Tail l) = Tail (f l)
    fmap f (Null l) = Null (f l)
    fmap f (Cons l r) = Cons (f l) (f r)
    fmap f (Append l r) = Append (f l) (f r)
