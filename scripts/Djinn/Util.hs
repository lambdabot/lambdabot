module Util where

lookupWithDefault :: (Eq a) => [(a, b)] -> b -> a -> b
lookupWithDefault ab b a =
    case lookup a ab of
    Nothing -> b
    Just b' -> b'

liftM1 :: (Functor f) => (a -> b) -> f a -> f b
liftM1 = fmap

