{-
Uses pattern guards

This is an interpreter of the Unlambda language, written in
the pure, lazy, functional language Haskell.

Copyright (C) 2001 by Ørjan Johansen <oerjan@nvg.ntnu.no>
Copyright (C) 2006 by Don Stewart - http://www.cse.unsw.edu.au/~dons

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Language.Unlambda where

------------------------------------------------------------------------
-- Abstract syntax

data Exp
    = App Exp Exp
    | K
    | K1 Exp
    | S
    | S1 Exp
    | S2 Exp Exp
    | I
    | V
    | C
    | Cont (Cont Exp)
    | D
    | D1 Exp
    | Dot Char
    | E
    | At
    | Ques Char
    | Pipe

------------------------------------------------------------------------
-- Printing

instance Show Exp where
  showsPrec _ = sh

sh :: Exp -> String -> String
sh (App x y)  = showChar '`' . sh x . sh y
sh K          = showChar 'k'
sh (K1 x)     = showString "`k" . sh x
sh S          = showChar 's'
sh (S1 x)     = showString "`s" . sh x
sh (S2 x y)   = showString "``s" . sh x . sh y
sh I          = showChar 'i'
sh V          = showChar 'v'
sh C          = showChar 'c'
sh (Cont _)   = showString "<cont>"
sh D          = showChar 'd'
sh (D1 x)     = showString "`d" . sh x
sh (Dot '\n') = showChar 'r'
sh (Dot c)    = showChar '.' . showChar c
sh E          = showChar 'e'
sh At         = showChar '@'
sh (Ques c)   = showChar '?' . showChar c
sh Pipe       = showChar '|'

------------------------------------------------------------------------
-- Eval monad

newtype Eval a = Eval ((Maybe Char, Int) -> Cont a -> IO Exp)

type Cont a = (Maybe Char, Int) -> a -> IO Exp

instance Monad Eval where

  (Eval cp1) >>= f = Eval $ \dat1 cont2 ->
                        cp1 dat1 $ \dat2 a ->
                            let (Eval cp2) = f a in cp2 dat2 cont2

  return a = Eval $ \dat cont -> cont dat a

------------------------------------------------------------------------
-- Basics

currentChar :: Eval (Maybe Char)
currentChar      = Eval (\dat@(c,_) cont -> cont dat c)
setCurrentChar :: Maybe Char -> Eval ()
setCurrentChar c = Eval (\(_,i) cont -> cont (c,i) ())
io :: IO a -> Eval a
io iocp          = Eval (\dat cont -> iocp >>= cont dat)
throw :: ((Maybe Char, Int) -> t -> IO Exp) -> t -> Eval a
throw c x        = Eval (\dat _ -> c dat x)
exit :: Exp -> Eval a
exit e           = Eval (\_ _ -> return e)
callCC :: (((Maybe Char, Int) -> a -> IO Exp) -> Eval a) -> Eval a
callCC f         = Eval $ \dat cont -> let Eval cp2 = f cont in cp2 dat cont
step :: Eval ()
step             = Eval (\(c,i) cont -> if i<1 then return E else cont (c,i-1) ())

------------------------------------------------------------------------
-- Interpretation in the Eval monad

eval :: Exp -> Eval Exp
eval (App e1 e2) = do
  f <- eval e1
  case f of
    D -> return (D1 e2)
    _ -> eval e2 >>= apply f
eval e = return e

apply :: Exp -> Exp -> Eval Exp
apply K x        = return (K1 x)
apply (K1 x) _   = return x
apply S x        = return (S1 x)
apply (S1 x) y   = return (S2 x y)
apply (S2 x y) z = eval (App (App x z) (App y z))
apply I x        = return x
apply V _        = return V
apply C x        = callCC (apply x . Cont)
apply (Cont c) x = throw c x
apply D x        = return x
apply (D1 e) x   = do f <- eval e; apply f x
apply (Dot c) x  = step >> io (putChar c) >> return x
apply E x        = exit x
apply At f = do
  dat <- io $ catch (fmap Just getChar) (const $ return Nothing)
  setCurrentChar dat
  apply f (case dat of Nothing -> V ; Just _  -> I)
apply (Ques c) f = do
  cur <- currentChar
  apply f (if cur == Just c then I else V)
apply Pipe f = do
  cur <- currentChar
  apply f (case cur of Nothing -> V ; Just c  -> Dot c)
apply (App _ _) _ = error "Unknown application"
