{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SimpleReflect
-- Copyright   :  (c) 2008 Twan van Laarhoven
-- License     :  BSD-style
-- 
-- Maintainer  :  twanvl@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple reflection of haskell expressions containing variables.
--
-----------------------------------------------------------------------------
module SimpleReflect
    ( Expr
    , var, fun, expr, reduce
    , a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    ) where


import Data.List
import Control.Applicative

{- -- Lennart Augustsson's extensions, temporarily disabled.
import Control.Monad.State hiding(lift)
-}

import Data.Typeable
import Data.Generics hiding (Fixity)


------------------------------------------------------------------------------
-- Data type
------------------------------------------------------------------------------

data Expr = Expr
   { showExpr   :: Int -> ShowS
   , intExpr    :: Maybe Integer
   , doubleExpr :: Maybe Double
   , reduced    :: Maybe Expr
   } deriving (Typeable, Data)

instance Show Expr where
    showsPrec p r = showExpr r p

-- Default expression
emptyExpr :: Expr
emptyExpr = Expr { showExpr   = \_ -> showString ""
                 , intExpr    = Nothing
                 , doubleExpr = Nothing
                 , reduced    = Nothing
                 }

------------------------------------------------------------------------------
-- Lifting and combining expressions
------------------------------------------------------------------------------

-- | A variable
var :: String -> Expr
var s = emptyExpr { showExpr = \_ -> showString s }

lift :: Show a => a -> Expr
lift x = emptyExpr { showExpr = \p -> showsPrec p x }

data Fixity = L | R deriving Eq

-- | A operator as expression
op :: Fixity -> Int -> String -> Expr -> Expr -> Expr
op fix prec op a b = emptyExpr { showExpr = showFun }
 where showFun p = showParen (p > prec)
                     $ showExpr a (if fix == L then prec else prec + 1)
                     . showString op
                     . showExpr b (if fix == R then prec else prec + 1)

------------------------------------------------------------------------------
-- Adding numeric results
------------------------------------------------------------------------------

iOp  r f a   = (r a  ) { intExpr    = f <$> intExpr    a }
iOp2 r f a b = (r a b) { intExpr    = f <$> intExpr    a <*> intExpr    b }
dOp  r f a   = (r a  ) { doubleExpr = f <$> doubleExpr a }
dOp2 r f a b = (r a b) { doubleExpr = f <$> doubleExpr a <*> doubleExpr b }

withReduce r a    = let rr = r a in
                    rr { reduced = withReduce r <$> reduced a
                               <|> fromInteger <$> intExpr    rr
                               <|> fromDouble  <$> doubleExpr rr
                       }
withReduce2 r a b = let rr = r a b in
                    rr { reduced = (\a' -> withReduce2 r a' b) <$> reduced a
                               <|> (\b' -> withReduce2 r a b') <$> reduced b
                               <|> fromInteger <$> intExpr    rr
                               <|> fromDouble  <$> doubleExpr rr
                       }

------------------------------------------------------------------------------
-- Function types
------------------------------------------------------------------------------

class FromExpr a where
    fromExpr :: Expr -> a

instance FromExpr Expr where
    fromExpr = id

instance (Show a, FromExpr b) => FromExpr (a -> b) where
    fromExpr f a = fromExpr $ op L 10 " " f (lift a)

fun :: FromExpr a => String -> a
fun = fromExpr . var

------------------------------------------------------------------------------
-- Variables!
------------------------------------------------------------------------------

a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z :: Expr
[a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
   = [var [x] | x <- ['a'..'e']++['i'..'z']]

f,g,h :: FromExpr a => a
f = fun "f"
g = fun "g"
h = fun "h"

------------------------------------------------------------------------------
-- Forcing conversion & evaluation
------------------------------------------------------------------------------

-- | Force something to be an expression
expr :: Expr -> Expr
expr = id

-- | Reduce (evaluate) an expression once
--   for example 1 + 2 + 3 + 4 ==> 3 + 3 + 4
reduce :: Expr -> Expr
reduce e = maybe e id (reduced e)

reduction :: Expr -> [Expr]
reduction e = e : unfoldr (\e -> do e' <- reduced e; return (e',e')) e

------------------------------------------------------------------------------
-- Numeric classes
------------------------------------------------------------------------------

instance Eq Expr where
    Expr{ intExpr    = Just a } == Expr{ intExpr    = Just b }  =  a == b
    Expr{ doubleExpr = Just a } == Expr{ doubleExpr = Just b }  =  a == b
    a                           == b                            =  show a == show b

instance Ord Expr where
    compare Expr{ intExpr    = Just a } Expr{ intExpr    = Just b }  =  compare a b
    compare Expr{ doubleExpr = Just a } Expr{ doubleExpr = Just b }  =  compare a b
    compare a                           b                            =  compare (show a) (show b)
    min = fun "min" `iOp2` min `dOp2` min
    max = fun "max" `iOp2` max `dOp2` max

instance Num Expr where
    (+)    = withReduce2 $ op L 6 " + " `iOp2` (+)   `dOp2` (+)
    (-)    = withReduce2 $ op L 6 " - " `iOp2` (-)   `dOp2` (-)
    (*)    = withReduce2 $ op L 7 " * " `iOp2` (*)   `dOp2` (*)
    negate = withReduce  $ fun "negate" `iOp` negate `dOp` negate
    abs    = withReduce  $ fun "abs"    `iOp` abs    `dOp` abs
    signum = withReduce  $ fun "signum" `iOp` signum `dOp` signum
    fromInteger i = (lift i)
                     { intExpr    = Just i
                     , doubleExpr = Just $ fromInteger i }

instance Real Expr where
    toRational expr = case (doubleExpr expr, intExpr expr) of
          (Just d,_) -> toRational d
          (_,Just i) -> toRational i
          _          -> error "not a number"

instance Integral Expr where
    quotRem a b = (quot a b, rem a b)
    divMod  a b = (div  a b, mod a b)
    quot = withReduce2 $ op L 7 " `quot` " `iOp2` quot
    rem  = withReduce2 $ op L 7 " `rem` "  `iOp2` rem
    div  = withReduce2 $ op L 7 " `div` "  `iOp2` div
    mod  = withReduce2 $ op L 7 " `mod` "  `iOp2` mod
    toInteger expr = case intExpr expr of
          Just i -> i
          _      -> error "not a number"

instance Fractional Expr where
    (/)   = withReduce2 $ op L 7 " / " `dOp2` (/)
    recip = withReduce  $ fun "recip"  `dOp` recip
    fromRational r = fromDouble (fromRational r)

fromDouble d = (lift d) { doubleExpr = Just d }

instance Floating Expr where
    pi    = (var "pi") { doubleExpr = Just pi }
    exp   = withReduce  $ fun "exp"   `dOp` exp
    sqrt  = withReduce  $ fun "sqrt"  `dOp` sqrt
    log   = withReduce  $ fun "log"   `dOp` log
    (**)  = withReduce2 $ op R 8 "**" `dOp2` (**)
    sin   = withReduce  $ fun "sin"   `dOp` sin
    cos   = withReduce  $ fun "cos"   `dOp` cos
    sinh  = withReduce  $ fun "sinh"  `dOp` sinh
    cosh  = withReduce  $ fun "cosh"  `dOp` cosh
    asin  = withReduce  $ fun "asin"  `dOp` asin
    acos  = withReduce  $ fun "acos"  `dOp` acos
    atan  = withReduce  $ fun "atan"  `dOp` atan
    asinh = withReduce  $ fun "asinh" `dOp` asinh
    acosh = withReduce  $ fun "acosh" `dOp` acosh
    atanh = withReduce  $ fun "atanh" `dOp` atanh

instance Enum Expr where
    succ   = withReduce  $ fun "succ" `iOp` succ `dOp` succ
    pred   = withReduce  $ fun "pred" `iOp` pred `dOp` pred
    toEnum = fun "toEnum"
    fromEnum = fromEnum . toInteger
    enumFrom       a     = map fromInteger $ enumFrom       (toInteger a)
    enumFromThen   a b   = map fromInteger $ enumFromThen   (toInteger a) (toInteger b)
    enumFromTo     a   c = map fromInteger $ enumFromTo     (toInteger a)               (toInteger c)
    enumFromThenTo a b c = map fromInteger $ enumFromThenTo (toInteger a) (toInteger b) (toInteger c)

instance Bounded Expr where
    minBound = var "minBound"
    maxBound = var "maxBound"

{- -- Lennart Augustsson's Extensions, temporarily disabled.

instance (Show a, ExprArg a, Show r) => Show (a -> r) where
    showsPrec _ f = showString "\\ " . showsPrec 0 v . showString " -> " .
                    showsPrec 0 (f v)
      where v = evalState exprArg vars
            dummy = evalState exprArg $ repeat "_"
            vars = supply \\ tokenize (show $ f dummy)
            supply = ["x","y","z"] ++ [ "x" ++ show i | i <- [1..]]
            tokenize "" = []
            tokenize s = case lex s of (x,s') : _ -> x : tokenize s'

class ExprArg a where
    exprArg :: State [String] a

instance ExprArg Expr where
    exprArg = do v:vs <- get; put vs; return (var v)

instance ExprArg () where
    exprArg = return ()

instance (ExprArg a, ExprArg b) => ExprArg (a, b) where
    exprArg = liftM2 (,) exprArg exprArg

instance (ExprArg a, ExprArg b, ExprArg c) => ExprArg (a, b, c) where
    exprArg = liftM3 (,,) exprArg exprArg exprArg
-}
