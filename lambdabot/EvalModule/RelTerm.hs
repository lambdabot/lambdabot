
module EvalModule.RelTerm where

import Control.Monad.Error

import EvalModule.LangPack
import EvalModule.ArithTerm (prjI)
import EvalModule.ListTerm (prjC,prjL,prjN)
import Data.Dynamic

data RelTerm term
   = Boolean !Bool
   | And term term
   | Or term term
   | Not term
   | Equal term term
   | NotEqual term term
   | LessThan term term
   | GreaterThan term term
   | LessThanOrEqual term term
   | GreaterThanOrEqual term term
   | IfE term term term

prjB :: (MonadError String m) => m Dynamic -> m Bool
prjB = prj'

doOp :: (MonadError String m, Typeable (m Dynamic)) => (forall a.Ord a => a -> a -> Bool) -> 
            m Dynamic -> m Dynamic -> m Dynamic
doOp op l r 
    = catchError (do l' <- prjI l; r' <- prjI r; return $ inj (l' `op` r'))
       (\_ -> catchError (do l' <- prjC l; r' <- prjC r; return $ inj (l' `op` r'))
        (\_ -> catchError (do l' <- prjB l; r' <- prjB r; return $ inj (l' `op` r'))
         (\_ -> catchError (do l' <- prjN l; r' <- prjN r; return $ inj (l' `op` r'))
          (\_ -> catchError (do _ <- prjL l; _ <- prjN r; return $ inj False)
           (\_ -> catchError (do _ <- prjN l; _ <- prjL r; return $ inj False)
            (\_ -> catchError (do (a,b) <- prjL l
                                  (x,y) <- prjL r
                                  s <- prjB $ doOp op a x
                                  t <- prjB $ doOp op b y
                                  return $ inj (s && t))
                   throwError))))))

phiRel :: (MonadError String m, Typeable (m Dynamic)) => RelTerm (m Dynamic) -> m Dynamic
phiRel (Boolean b) = return (inj b)
phiRel (IfE c t e) = do b <- prjB c
                        if b then t else e
phiRel (Equal l r) = doOp (==) l r
phiRel (NotEqual l r) = doOp (/=) l r
phiRel (LessThan l r) = doOp (<) l r
phiRel (GreaterThan l r) = doOp (>) l r
phiRel (LessThanOrEqual l r) = doOp (<=) l r
phiRel (GreaterThanOrEqual l r) = doOp (>=) l r
phiRel (And l r) = do l' <- prjB l; if not l' then return $ inj False else do r' <- prjB r; return $ inj (l' && r')
phiRel (Or l r) = do l' <- prjB l; if l' then return $ inj True else do r' <- prjB r; return $ inj (l' || r')
phiRel (Not x) = do x' <- prjB x; return $ inj (not x')

instance Functor RelTerm where
    fmap f (Not x) = Not (f x)
    fmap f (Or l r) = Or (f l) (f r)
    fmap f (And l r) = And (f l) (f r)
    fmap _ (Boolean b) = (Boolean b)
    fmap f (IfE c t e) = IfE (f c) (f t) (f e)
    fmap f (Equal l r) = Equal (f l) (f r)
    fmap f (NotEqual l r) = NotEqual (f l) (f r)
    fmap f (LessThan l r) = LessThan (f l) (f r)
    fmap f (GreaterThan l r) = GreaterThan (f l) (f r)
    fmap f (LessThanOrEqual l r) = LessThanOrEqual (f l) (f r)
    fmap f (GreaterThanOrEqual l r) = GreaterThanOrEqual (f l) (f r)
