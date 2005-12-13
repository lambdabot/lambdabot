module LJTFormula(Symbol(..), Formula(..), (<->), (&), (|:), fnot,
	Term(..), applys
	) where

infixr 2 :->
infix  2 <->
infixl 3 |:
infixl 4 &

newtype Symbol = Symbol String
     deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

data Formula
	= Conj [Formula]
	| Disj [Formula]
	| Formula :-> Formula
	| Falsity
	| Truth
	| PVar Symbol
     deriving (Eq, Ord)

(<->) :: Formula -> Formula -> Formula
x <-> y = (x:->y) & (y:->x)

(&) :: Formula -> Formula -> Formula
x & y = Conj [x, y]

(|:) :: Formula -> Formula -> Formula
x |: y = Disj [x, y]

fnot :: Formula -> Formula
fnot x = x :-> Falsity

-- Show formulae the LJT way
instance Show Formula where
    showsPrec p (Conj cs) =
	showParen (p>40) $ loop cs
	  where loop [f] = showsPrec 41 f
		loop (f : fs) = showsPrec 41 f . showString " & " . loop fs
		loop [] = error "showsPrec Conj"
    showsPrec p (Disj ds) =
	showParen (p>30) $ loop ds
	  where loop [f] = showsPrec 31 f
		loop (f : fs) = showsPrec 31 f . showString " v " . loop fs
		loop [] = error "showsPrec Disj"
    showsPrec _ (f1 :-> Falsity) =
	showString "~" . showsPrec 100 f1
    showsPrec p (f1 :-> f2) =
	showParen (p>20) $ showsPrec 21 f1 . showString " -> " . showsPrec 20 f2
    showsPrec _ Falsity = showString "false"
    showsPrec _ Truth = showString "true"
    showsPrec p (PVar s) = showsPrec p s

------------------------------

data Term
	= Var Symbol
	| Lam Symbol Term
	| Apply Term Term
	| Cunit
	| Cabsurd
	| Ctuple Int
	| Csplit Int
	| Cinj Int
	| Ccases Int
    deriving (Eq)

instance Show Term where
    showsPrec p (Var s) = showsPrec p s
    showsPrec p (Lam s e) = showParen (p > 0) $ showString "\\" . showsPrec 0 s . showString "." . showsPrec 0 e
    showsPrec p (Apply f a) = showParen (p > 1) $ showsPrec 1 f . showString " " . showsPrec 2 a
    showsPrec _ (Cinj i) = showString $ "Inj" ++ show i
    showsPrec _ (Ctuple _) = showString "Pair"
    showsPrec _ Cunit = showString "Unit"
    showsPrec _ Cabsurd = showString "absurd"
    showsPrec _ (Csplit _) = showString "split"
    showsPrec _ (Ccases n) = showString $ "cases" ++ show n

applys :: Term -> [Term] -> Term
applys f as = foldl Apply f as

