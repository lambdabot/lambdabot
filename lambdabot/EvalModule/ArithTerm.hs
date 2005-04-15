
module EvalModule.ArithTerm where

import Control.Monad.Error
import EvalModule.LangPack
import Data.Dynamic

data ArithTerm term
   = Num !Integer
   | Neg term
   | Add term term
   | Sub term term
   | Mul term term
   | Div term term

prjI :: (MonadError String m) => m Dynamic -> m Integer
prjI = prj'

phiArith :: (MonadError String m) => ArithTerm (m Dynamic) -> m Dynamic
phiArith (Num n) = return (inj n)
phiArith (Neg n) = do n' <- prjI n; return $ inj (-n')
phiArith (Add l r) = do l' <- prjI l
                        r' <- prjI r
                        return $ inj (l' + r')
phiArith (Sub l r) = do l' <- prjI l
                        r' <- prjI r
                        return $ inj (l' - r')
phiArith (Mul l r) = do l' <- prjI l
                        r' <- prjI r
                        return $ inj (l' * r')
phiArith (Div l r) = do l' <- prjI l
                        r' <- prjI r
                        if r' == 0 then throwError "divide by zero"
                                   else return $ inj (l' `div` r')

instance Functor ArithTerm where
    fmap _ (Num n) = Num n
    fmap f (Neg n) = Neg (f n)
    fmap f (Add l r) = Add (f l) (f r)
    fmap f (Sub l r) = Sub (f l) (f r)
    fmap f (Mul l r) = Mul (f l) (f r)
    fmap f (Div l r) = Div (f l) (f r)
