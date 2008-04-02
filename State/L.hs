{-# LANGUAGE CPP #-}
module L where

#include "imports.h"

describeSequence = fmap description . lookupSequence 

infixr 9 .

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

infixr 5 ++

(++) :: Monoid m => m -> m -> m
(++) = mappend

zero :: Monoid m => m
zero = mempty

{-# LINE 1 "<local>" #-}
