module LJTFormula(Symbol(..), Formula(..), (<->), (&), fnot, hsShowFormula,
	Term(..), applys, hsShowTerm
	) where

infixr 2 :->
infix  2 <->
infixl 3 :|
infixl 4 &

newtype Symbol = Symbol String
     deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

data Formula
	= Conj [Formula]
	| Formula :| Formula
	| Formula :-> Formula
	| Falsity
	| Truth
	| PVar Symbol
     deriving (Eq, Ord)

(<->) :: Formula -> Formula -> Formula
x <-> y = (x:->y) & (y:->x)

(&) :: Formula -> Formula -> Formula
x & y = Conj [x, y]

fnot :: Formula -> Formula
fnot x = x :-> Falsity

-- Show formulae the LJT way
instance Show Formula where
    showsPrec p (Conj cs) =
	showParen (p>40) $ loop cs
	  where loop [f] = showsPrec 41 f
		loop (f : fs) = showsPrec 41 f . showString " & " . loop fs
		loop [] = error "showsPrec Conj"
    showsPrec p (f1 :|  f2) =
	showParen (p>30) $ showsPrec 30 f1 . showString " v "  . showsPrec 31 f2
    showsPrec _ (f1 :-> Falsity) =
	showString "~" . showsPrec 100 f1
    showsPrec p (f1 :-> f2) =
	showParen (p>20) $ showsPrec 21 f1 . showString " -> " . showsPrec 20 f2
    showsPrec _ Falsity = showString "false"
    showsPrec _ Truth = showString "true"
    showsPrec p (PVar s) = showsPrec p s

hsShowFormula :: Formula -> String
hsShowFormula form = hpr 0 form ""
  where	hpr _ (Conj cs) = showParen True $ withSep " " (map (hpr 0) cs)
	hpr p (f1 :|  f2) = showParen (p>50) $ showString "Either " . hpr 51 f1 . showString " " . hpr 51 f2
	hpr p (f1 :-> f2) = showParen (p>20) $ hpr 21 f1 . showString " -> " . hpr 20 f2
	hpr _ Falsity = showString "Void"
	hpr _ Truth = showString "()"
	hpr p (PVar s) = showsPrec p s

withSep :: String -> [ShowS] -> ShowS
withSep _ [] = error "withSep"
withSep _ [f] = f
withSep sep (f : fs) = f . showString sep . withSep sep fs

------------------------------

data Term
	= Var Symbol
	| Lam Symbol Term
	| Apply Term Term
	| Cleft
	| Cright
	| Cunit
	| Ctuple Int
	| Cabsurd
	| Csplit Int
	| Ceither
    deriving (Eq)

instance Show Term where
    showsPrec p (Var s) = showsPrec p s
    showsPrec p (Lam s e) = showParen (p > 0) $ showString "\\" . showsPrec 0 s . showString "." . showsPrec 0 e
    showsPrec p (Apply f a) = showParen (p > 1) $ showsPrec 1 f . showString " " . showsPrec 2 a
    showsPrec _ Cleft = showString "Left"
    showsPrec _ Cright = showString "Right"
    showsPrec _ (Ctuple _) = showString "Pair"
    showsPrec _ Cunit = showString "Unit"
    showsPrec _ Cabsurd = showString "absurd"
    showsPrec _ (Csplit _) = showString "split"
    showsPrec _ Ceither = showString "either"

applys :: Term -> [Term] -> Term
applys f as = foldl Apply f as

hsShowTerm :: Term -> String
hsShowTerm t = hpr 0 t ""
  where	hpr p (Var s) = showsPrec p s
	hpr p (Lam s e) = showParen (p > 0) $ showString "\\" . showsPrec 0 s . showString " -> " . hpr 0 e
	hpr p (Apply fun arg) = hprApp fun [arg]
	  where hprApp (Apply f a) as = hprApp f (a : as)
		hprApp (Ctuple n) as = if length as /= n then error "hpr Ctuple" else withSep ", " (map (hpr 0) as)
		hprApp f as = showParen (p > 1) $ withSep " " (hpr 1 f : map (hpr 2) as)
	hpr _ Cleft = showString "Left"
	hpr _ Cright = showString "Right"
	hpr _ Cunit = showString "()"
	hpr _ (Ctuple n) = showParen True $ showString $ replicate (n-1) ','
	hpr _ Cabsurd = showString "void"
	hpr _ (Csplit n) = showString "split" . showsPrec 0 n
	hpr _ Ceither = showString "either"
