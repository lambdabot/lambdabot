{-# OPTIONS -fvia-C #-}

module Plugin.Pl.Common (
        Fixity(..), Expr(..), Pattern(..), Decl(..), TopLevel(..),
        bt, sizeExpr, mapTopLevel, getExpr,
        operators, opchars, reservedOps, lookupOp, lookupFix, minPrec, maxPrec,
        comp, flip', id', const', scomb, cons, nil, fix', if',
        makeList, getList,
        Assoc(..),
        module Data.Maybe,
        module Control.Arrow,
        module Data.List,
        module Control.Monad,
        module GHC.Base
    ) where

import Data.Maybe (isJust, fromJust)
import Data.List (intersperse, minimumBy)
import qualified Data.Map as M

import Control.Monad
import Control.Arrow (first, second, (***), (&&&), (|||), (+++))

import Text.ParserCombinators.Parsec.Expr (Assoc(..))

import GHC.Base (assert)


-- The rewrite rules can be found at the end of the file Rules.hs

-- Not sure if passing the information if it was used as infix or prefix
-- is worth threading through the whole thing is worth the effort,
-- but it stays that way until the prettyprinting algorithm gets more
-- sophisticated.
data Fixity = Pref | Inf deriving Show

instance Eq Fixity where
  _ == _ = True

instance Ord Fixity where
  compare _ _ = EQ

data Expr
  = Var Fixity String
  | Lambda Pattern Expr
  | App Expr Expr
  | Let [Decl] Expr
  deriving (Eq, Ord)

data Pattern
  = PVar String 
  | PCons Pattern Pattern
  | PTuple Pattern Pattern
  deriving (Eq, Ord)

data Decl = Define { 
  declName :: String, 
  declExpr :: Expr
} deriving (Eq, Ord)

data TopLevel = TLD Bool Decl | TLE Expr deriving (Eq, Ord)

mapTopLevel :: (Expr -> Expr) -> TopLevel -> TopLevel
mapTopLevel f tl = case getExpr tl of (e, c) -> c $ f e

getExpr :: TopLevel -> (Expr, Expr -> TopLevel)
getExpr (TLD True (Define foo e)) = (Let [Define foo e] (Var Pref foo), 
                                     \e' -> TLD False $ Define foo e')
getExpr (TLD False (Define foo e)) = (e, \e' -> TLD False $ Define foo e')
getExpr (TLE e)      = (e, TLE)

sizeExpr :: Expr -> Int
sizeExpr (Var _ _) = 1
sizeExpr (App e1 e2) = sizeExpr e1 + sizeExpr e2 + 1
sizeExpr (Lambda _ e) = 1 + sizeExpr e
sizeExpr (Let ds e) = 1 + sum (map sizeDecl ds) + sizeExpr e where
  sizeDecl (Define _ e') = 1 + sizeExpr e'

comp, flip', id', const', scomb, cons, nil, fix', if' :: Expr
comp   = Var Inf  "."
flip'  = Var Pref "flip"
id'    = Var Pref "id"
const' = Var Pref "const"
scomb  = Var Pref "ap"
cons   = Var Inf  ":"
nil    = Var Pref "[]"
fix'   = Var Pref "fix"
if'    = Var Pref "if'"

makeList :: [Expr] -> Expr
makeList = foldr (\e1 e2 -> cons `App` e1 `App` e2) nil

-- Modularity is a drag
getList :: Expr -> ([Expr], Expr)
getList (c `App` x `App` tl) | c == cons = first (x:) $ getList tl
getList e = ([],e)

bt :: a
bt = undefined

shift, minPrec, maxPrec :: Int
shift = 0
maxPrec = shift + 10
minPrec = 0

-- operator precedences are needed both for parsing and prettyprinting
operators :: [[(String, (Assoc, Int))]]
operators = (map . map . second . second $ (+shift))
  [[inf "." AssocRight 9, inf "!!" AssocLeft 9],
   [inf name AssocRight 8 | name <- ["^", "^^", "**"]],
   [inf name AssocLeft 7
     | name <- ["*", "/", "`quot`", "`rem`", "`div`", "`mod`", ":%", "%"]],
   [inf name AssocLeft 6  | name <- ["+", "-"]],
   [inf name AssocRight 5 | name <- [":", "++", "<+>"]],
   [inf name AssocNone 4
     | name <- ["==", "/=", "<", "<=", ">=", ">", "`elem`", "`notElem`"]] ++[inf name AssocLeft 4 | name <- ["<*","*>","<$>","<$","<**>"]],
   [inf "&&" AssocRight 3, inf "***" AssocRight 3, inf "&&&" AssocRight 3, inf "<|>" AssocLeft 3],
   [inf "||" AssocRight 2, inf "+++" AssocRight 2, inf "|||" AssocRight 2],
   [inf ">>" AssocLeft 1, inf ">>=" AssocLeft 1, inf "=<<" AssocRight 1, inf ">>>" AssocRight 1, inf "^>>" AssocRight 1, inf "^<<" AssocRight 1],
   [inf name AssocRight 0 | name <- ["$", "$!", "`seq`"]]
  ] where
  inf name assoc fx = (name, (assoc, fx))

opchars :: [Char]
opchars = "!@#$%^*./|=-+:?<>&"

reservedOps :: [String]
reservedOps = ["->", "..", "="]

opFM :: M.Map String (Assoc, Int)
opFM = (M.fromList $ concat operators)

lookupOp :: String -> Maybe (Assoc, Int)
lookupOp k = M.lookup k opFM

lookupFix :: String -> (Assoc, Int)
lookupFix str = case lookupOp $ str of
  Nothing -> (AssocLeft, 9 + shift)
  Just x  -> x
