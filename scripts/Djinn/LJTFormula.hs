--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--
module LJTFormula(Symbol(..), Formula(..), (<->), (&), (|:), fnot, false, true,
        ConsDesc(..),
        Term(..), applys, freeVars
        ) where
import Data.List(union, (\\))

infixr 2 :->
infix  2 <->
infixl 3 |:
infixl 4 &

newtype Symbol = Symbol String
     deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol s) = s

data ConsDesc = ConsDesc String Int     -- name and arity
     deriving (Eq, Ord, Show)

data Formula
        = Conj [Formula]
        | Disj [(ConsDesc, Formula)]
        | Formula :-> Formula
        | PVar Symbol
     deriving (Eq, Ord)

(<->) :: Formula -> Formula -> Formula
x <-> y = (x:->y) & (y:->x)

(&) :: Formula -> Formula -> Formula
x & y = Conj [x, y]

(|:) :: Formula -> Formula -> Formula
x |: y = Disj [((ConsDesc "Left" 1), x), ((ConsDesc "Right" 1), y)]

fnot :: Formula -> Formula
fnot x = x :-> false

false :: Formula
false = Disj []

true :: Formula
true = Conj []

-- Show formulae the LJT way
instance Show Formula where
    showsPrec _ (Conj []) = showString "true"
    showsPrec _ (Conj [c]) = showParen True $ showString "&" . showsPrec 0 c
    showsPrec p (Conj cs) =
        showParen (p>40) $ loop cs
          where loop [f] = showsPrec 41 f
                loop (f : fs) = showsPrec 41 f . showString " & " . loop fs
                loop [] = error "showsPrec Conj"
    showsPrec _ (Disj []) = showString "false"
    showsPrec _ (Disj [(_,c)]) = showParen True $ showString "|" . showsPrec 0 c
    showsPrec p (Disj ds) =
        showParen (p>30) $ loop ds
          where loop [(_,f)] = showsPrec 31 f
                loop ((_,f) : fs) = showsPrec 31 f . showString " v " . loop fs
                loop [] = error "showsPrec Disj"
    showsPrec _ (f1 :-> Disj []) =
        showString "~" . showsPrec 100 f1
    showsPrec p (f1 :-> f2) =
        showParen (p>20) $ showsPrec 21 f1 . showString " -> " . showsPrec 20 f2
    showsPrec p (PVar s) = showsPrec p s

------------------------------

data Term
        = Var Symbol
        | Lam Symbol Term
        | Apply Term Term
        | Ctuple Int
        | Csplit Int
        | Cinj ConsDesc Int
        | Ccases [ConsDesc]
        | Xsel Int Int Term             --- XXX just temporary by MJ
    deriving (Eq, Ord)

instance Show Term where
    showsPrec p (Var s) = showsPrec p s
    showsPrec p (Lam s e) = showParen (p > 0) $ showString "\\" . showsPrec 0 s . showString "." . showsPrec 0 e
    showsPrec p (Apply f a) = showParen (p > 1) $ showsPrec 1 f . showString " " . showsPrec 2 a
    showsPrec _ (Cinj _ i) = showString $ "Inj" ++ show i
    showsPrec _ (Ctuple i) = showString $ "Tuple" ++ show i
    showsPrec _ (Csplit n) = showString $ "split" ++ show n
    showsPrec _ (Ccases cds) = showString $ "cases" ++ show (length cds)
    showsPrec p (Xsel i n e) = showParen (p > 0) $ showString ("sel_" ++ show i ++ "_" ++ show n) . showString " " . showsPrec 2 e

applys :: Term -> [Term] -> Term
applys f as = foldl Apply f as

freeVars :: Term -> [Symbol]
freeVars (Var s) = [s]
freeVars (Lam s e) = freeVars e \\ [s]
freeVars (Apply f a) = freeVars f `union` freeVars a
freeVars (Xsel _ _ e) = freeVars e
freeVars _ = []
