{-# OPTIONS -fglasgow-exts #-}
-- ^ For multi-parameter classes
--
-- Based off of (ripping) ideas from 
--          "Language Prototyping using Modular Monadic Semantics"
--
module EvalModule.LangPack where

import Control.Monad.Error
import Text.ParserCombinators.Parsec.Prim

import Data.Dynamic

-- import Debug.Trace

--TODO: remove Debug.Trace,trace

newtype Fix f = In { out :: f (Fix f) }

fold :: (Functor f) => (f a -> a) -> Fix f -> a
fold phi = phi . fmap (fold phi) . out

(@@) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
f @@ g = \x -> g x >>= f

mFold :: (MFunctor f m) => (f a -> m a) -> Fix f -> m a
mFold mPhi = mPhi @@ mMap (mFold mPhi) @@ (return . out)

class (Functor f, Monad m) => MFunctor f m where
    mMap :: (a -> m b) -> f a -> m (f b)

class Pause m r | m -> r where
    pause :: ((a -> r) -> r) -> m a

--------------------------------------------------------------------------------

{- bugger it, I'll just use Dynamics
-- Functional dependencies may help
-- Extensible sums (as seen in many places e.g.
--            "Monad Transformers and Modular Interpreters")
class Subtype sub sup where
    inj :: sub -> sup
    prj :: sup -> Maybe sub

instance Subtype a (Either a b) where
    inj = Left
    prj = either Just (const Nothing)

instance Subtype a b => Subtype a (Either c b) where
    inj = Right . inj
    prj = either (const Nothing) prj
-}

-- prj' :: (Subtype sub sup, MonadError String m) => m sup -> m sub
prj' :: (MonadError String m, Typeable a) => m Dynamic -> m a
prj' x = do x' <- x
            case {-trace (show x')-} prj x' of
               Nothing -> throwError "type error"
               Just xr -> return xr

inj :: (Typeable a) => a -> Dynamic
inj = toDyn

prj :: (Typeable a) => Dynamic -> Maybe a
prj = fromDynamic

newtype SumF f g x = S { unS :: Either (f x) (g x) }

instance (Functor f, Functor g) => Functor (SumF f g) where
    fmap f = S . either (Left . fmap f) (Right . fmap f) . unS

--------------------------------------------------------------------------------
-- More Labragayo, but with a some of my own concoctions
{-
class HasParser t where
    parser :: Parser t

class HasParserF f where
    parserF :: (f x -> x) -> Parser x -> Parser (f x)

instance (HasParserF f, HasParserF g) => HasParserF (SumF f g) where
    parserF up parser = fmap S $ try (fmap Left (parserF (up . S . Left) parser)) <|>
                                      fmap Right (parserF (up . S . Right) parser)

instance (HasParserF f) => HasParser (Fix f) where
    parser = fmap In (parserF In parser)
-}    
