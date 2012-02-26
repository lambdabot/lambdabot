
--
-- | Names of haskell functions used in the Pl code
--
module Plugin.Pl.Names where

import Plugin.Pl.Common


-- | Expressions with holes
-- No MLambda here because we only consider closed Terms (no alpha-renaming!).
-- Has to be in this module, otherwise we get recursion
data MExpr
  = MApp  !MExpr !MExpr  -- ^ Application
  | Hole  !Int           -- ^ Hole/argument where another expression could go
  | Quote !Expr
  deriving Eq


-- Names
idE, flipE, bindE, extE, returnE, consE, appendE, nilE, foldrE, foldlE, fstE,
  sndE, dollarE, constE, uncurryE, curryE, compE, headE, tailE, sE, commaE, 
  fixE, foldl1E, notE, equalsE, nequalsE, plusE, multE, zeroE, oneE, lengthE, 
  sumE, productE, concatE, concatMapE, joinE, mapE, fmapE, fmapIE, subtractE, 
  minusE, liftME, apE, liftM2E, seqME, zipE, zipWithE, 
  crossE, firstE, secondE, andE, orE, allE, anyE :: MExpr
idE        = Quote $ Var Pref "id"
flipE      = Quote $ Var Pref "flip"
constE     = Quote $ Var Pref "const"
compE      = Quote $ Var Inf  "."
sE         = Quote $ Var Pref "ap"
fixE       = Quote $ Var Pref "fix"
bindE      = Quote $ Var Inf  ">>="
extE       = Quote $ Var Inf  "=<<"
returnE    = Quote $ Var Pref "return"
consE      = Quote $ Var Inf  ":"
nilE       = Quote $ Var Pref "[]"
appendE    = Quote $ Var Inf  "++"
foldrE     = Quote $ Var Pref "foldr"
foldlE     = Quote $ Var Pref "foldl"
fstE       = Quote $ Var Pref "fst"
sndE       = Quote $ Var Pref "snd"
dollarE    = Quote $ Var Inf  "$"
uncurryE   = Quote $ Var Pref "uncurry"
curryE     = Quote $ Var Pref "curry"
headE      = Quote $ Var Pref "head"
tailE      = Quote $ Var Pref "tail"
commaE     = Quote $ Var Inf  ","
foldl1E    = Quote $ Var Pref "foldl1"
equalsE    = Quote $ Var Inf  "=="
nequalsE   = Quote $ Var Inf  "/="
notE       = Quote $ Var Pref "not"
plusE      = Quote $ Var Inf  "+"
multE      = Quote $ Var Inf  "*"
zeroE      = Quote $ Var Pref "0"
oneE       = Quote $ Var Pref "1"
lengthE    = Quote $ Var Pref "length"
sumE       = Quote $ Var Pref "sum"
productE   = Quote $ Var Pref "product"
concatE    = Quote $ Var Pref "concat"
concatMapE = Quote $ Var Pref "concatMap"
joinE      = Quote $ Var Pref "join"
mapE       = Quote $ Var Pref "map"
fmapE      = Quote $ Var Pref "fmap"
fmapIE     = Quote $ Var Inf  "fmap"
subtractE  = Quote $ Var Pref "subtract"
minusE     = Quote $ Var Inf  "-"
liftME     = Quote $ Var Pref "liftM"
liftM2E    = Quote $ Var Pref "liftM2"
apE        = Quote $ Var Inf  "ap"
seqME      = Quote $ Var Inf  ">>"
zipE       = Quote $ Var Pref "zip"
zipWithE   = Quote $ Var Pref "zipWith"
crossE     = Quote $ Var Inf  "***"
firstE     = Quote $ Var Pref "first"
secondE    = Quote $ Var Pref "second"
andE       = Quote $ Var Pref "and"
orE        = Quote $ Var Pref "or"
allE       = Quote $ Var Pref "all"
anyE       = Quote $ Var Pref "any"



a, c :: MExpr -> MExpr -> MExpr
a       = MApp
c e1 e2 = compE `a` e1 `a` e2
infixl 9 `a`
infixr 8 `c`

