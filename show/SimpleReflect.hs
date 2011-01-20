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
-- Simple reflection of Haskell expressions containing variables.
--
-----------------------------------------------------------------------------
module SimpleReflect
    ( Expr
    , var, fun, expr, reduce
    , a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
    ) where

import Control.Applicative

{- -- Lennart Augustsson's extensions, temporarily disabled.
import Control.Monad.State hiding(lift)
-}

import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Generics (Typeable, Data)


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
    showsPrec pp rr = showExpr rr pp

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
var ss = emptyExpr { showExpr = \_ -> showString ss }

lift :: Show a => a -> Expr
lift xx = emptyExpr { showExpr = (`showsPrec` xx) }

data Fixity = L | R deriving Eq

-- | A operator as expression
op :: Fixity -> Int -> String -> Expr -> Expr -> Expr
op fix prec opp aa bb = emptyExpr { showExpr = showFun }
 where showFun pp = showParen (pp > prec)
                     $ showExpr aa (if fix == L then prec else prec + 1)
                     . showString opp
                     . showExpr bb (if fix == R then prec else prec + 1)

------------------------------------------------------------------------------
-- Adding numeric results
------------------------------------------------------------------------------
iOp :: (Expr -> Expr) -> (Integer -> Integer) -> Expr -> Expr
iOp  rr ff aa   = (rr a  ) { intExpr    = ff <$> intExpr    aa }
iOp2 :: (Expr -> Expr -> Expr) -> (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
iOp2 rr ff aa bb = (rr aa bb) { intExpr    = ff <$> intExpr    aa <*> intExpr    bb }
dOp :: (Expr -> Expr) -> (Double -> Double) -> Expr -> Expr
dOp  rr ff aa   = (rr aa  ) { doubleExpr = ff <$> doubleExpr aa }
dOp2 :: (Expr -> Expr -> Expr) -> (Double -> Double -> Double) -> Expr -> Expr -> Expr
dOp2 rr ff aa bb = (rr aa bb) { doubleExpr = ff <$> doubleExpr aa <*> doubleExpr bb }

withReduce :: (Expr -> Expr) -> Expr -> Expr
withReduce rr aa    = let rrr = rr aa in
                    rrr { reduced = withReduce rr <$> reduced aa
                               <|> fromInteger <$> intExpr    rrr
                               <|> fromDouble  <$> doubleExpr rrr
                       }
withReduce2 :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
withReduce2 rr aa bb = let rrr = rr aa bb in
                    rrr { reduced = (\aa' -> withReduce2 rr aa' b) <$> reduced aa
                               <|> withReduce2 rr aa <$> reduced bb
                               <|> fromInteger <$> intExpr    rrr
                               <|> fromDouble  <$> doubleExpr rrr
                       }

------------------------------------------------------------------------------
-- Function types
------------------------------------------------------------------------------

class FromExpr a where
    fromExpr :: Expr -> a

instance FromExpr Expr where
    fromExpr = id

instance (Show a, FromExpr b) => FromExpr (a -> b) where
    fromExpr ff aa = fromExpr $ op L 10 " " ff (lift aa)

fun :: FromExpr a => String -> a
fun = fromExpr . var

------------------------------------------------------------------------------
-- Variables!
------------------------------------------------------------------------------

a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z :: Expr
[a,b,c,d,e,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]
   = [var [xx] | xx <- ['a'..'e']++['i'..'z']]

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
reduce ee = fromMaybe ee (reduced ee)

------------------------------------------------------------------------------
-- Numeric classes
------------------------------------------------------------------------------

instance Eq Expr where
    Expr{ intExpr    = Just aa } == Expr{ intExpr    = Just bb }  =  aa == bb
    Expr{ doubleExpr = Just aa } == Expr{ doubleExpr = Just bb }  =  aa == bb
    aa                           == bb                            =  show aa == show bb

instance Ord Expr where
    compare Expr{ intExpr    = Just aa } Expr{ intExpr    = Just bb }  =  compare aa bb
    compare Expr{ doubleExpr = Just aa } Expr{ doubleExpr = Just bb }  =  compare aa bb
    compare aa                           bb                            =  comparing show aa bb
    min = fun "min" `iOp2` min `dOp2` min
    max = fun "max" `iOp2` max `dOp2` max

instance Num Expr where
    (+)    = withReduce2 $ op L 6 " + " `iOp2` (+)   `dOp2` (+)
    (-)    = withReduce2 $ op L 6 " - " `iOp2` (-)   `dOp2` (-)
    (*)    = withReduce2 $ op L 7 " * " `iOp2` (*)   `dOp2` (*)
    negate = withReduce  $ fun "negate" `iOp` negate `dOp` negate
    abs    = withReduce  $ fun "abs"    `iOp` abs    `dOp` abs
    signum = withReduce  $ fun "signum" `iOp` signum `dOp` signum
    fromInteger ii = (lift ii)
                     { intExpr    = Just ii
                     , doubleExpr = Just $ fromInteger ii }

instance Real Expr where
    toRational xpr = case (doubleExpr xpr, intExpr xpr) of
          (Just dd,_) -> toRational dd
          (_,Just ii) -> toRational ii
          _          -> error "not a number"

instance Integral Expr where
    quotRem aa bb = (quot aa bb, rem aa bb)
    divMod  aa bb = (div aa bb, mod aa bb)
    quot = withReduce2 $ op L 7 " `quot` " `iOp2` quot
    rem  = withReduce2 $ op L 7 " `rem` "  `iOp2` rem
    div  = withReduce2 $ op L 7 " `div` "  `iOp2` div
    mod  = withReduce2 $ op L 7 " `mod` "  `iOp2` mod
    toInteger xpr = case intExpr xpr of
          Just ii -> ii
          _      -> error "not a number"

instance Fractional Expr where
    (/)   = withReduce2 $ op L 7 " / " `dOp2` (/)
    recip = withReduce  $ fun "recip"  `dOp` recip
    fromRational rr = fromDouble (fromRational rr)

fromDouble :: Double -> Expr
fromDouble dd = (lift dd) { doubleExpr = Just dd }

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
    enumFrom       aa     = map fromInteger $ enumFrom       (toInteger aa)
    enumFromThen   aa bb  = map fromInteger $ enumFromThen (toInteger aa) (toInteger bb)
    enumFromTo     aa  cc = map fromInteger $ enumFromTo (toInteger aa) (toInteger cc)
    enumFromThenTo aa bb cc = map fromInteger $ enumFromThenTo (toInteger aa) (toInteger bb) (toInteger cc)

instance Bounded Expr where
    minBound = var "minBound"
    maxBound = var "maxBound"

{- -- Lennart Augustsson's Extensions, temporarily disabled.
See <http://augustss.blogspot.com/2008/03/in-recent-blog-post-by-twan-van.html>.

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
