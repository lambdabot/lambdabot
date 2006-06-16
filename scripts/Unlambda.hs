{-# OPTIONS -fglasgow-exts #-}
-- 
-- Uses pattern guards
--
-- This is an interpreter of the Unlambda language, written in
-- the pure, lazy, functional language Haskell.
-- 
-- Copyright (C) 2001 by Ørjan Johansen <oerjan@nvg.ntnu.no>
-- Copyright (C) 2006 by Don Stewart - http://www.cse.unsw.edu.au/~dons
--                                                                           
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--                                                                           
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--                                                                           
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

--
-- A time- and output-limited unlambda
--

import Data.Char
import System.IO
import System.Posix.Resource

rlimit = ResourceLimit 3

main = setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit) >> run

run = do
  exp <- parse stdin
  let (Eval cp) = eval exp
  cp (Nothing, 2048) (const return)

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
-- Parsing of the Unlambda program directly from handle
--
parse :: Handle -> IO Exp
parse h = do
  c <- catch (hGetChar h) (\_ -> error "Parse error at end of file")
  case toLower c of
    c | c `elem` " \t\n"  -> parse h
    '`' -> do e1 <- parse h
              e2 <- parse h
              return (App e1 e2)
    '#' -> hGetLine h >> parse h
    '.' -> hGetChar h >>= return . Dot
    '?' -> hGetChar h >>= return . Ques
    _ | Just fn <- lookup c table -> return fn
      | otherwise                 -> error $ "Unknown operator " ++ show c

    where table = zip "ksivcdre@|" [K,S,I,V,C,D,Dot '\n',E,At,Pipe]

------------------------------------------------------------------------
-- Printing

instance Show Exp where showsPrec _ e = sh e

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

currentChar      = Eval (\dat@(c,_) cont -> cont dat c)
setCurrentChar c = Eval (\(_,i) cont -> cont (c,i) ())
io iocp          = Eval (\dat cont -> iocp >>= cont dat)
throw c x        = Eval (\dat cont -> c dat x)
exit e           = Eval (\_ _ -> return e)
callCC f         = Eval $ \dat cont -> let Eval cp2 = f cont in cp2 dat cont
step             = Eval (\(c,i) cont -> if i<1 then return E else cont (c,i-1) ())

------------------------------------------------------------------------
-- Interpretation in the Eval monad

eval (App e1 e2) = do
  f <- eval e1
  case f of
    D -> return (D1 e2)
    _ -> eval e2 >>= apply f
eval e = return e

apply K x        = return (K1 x)
apply (K1 x) y   = return x
apply S x        = return (S1 x)
apply (S1 x) y   = return (S2 x y)
apply (S2 x y) z = eval (App (App x z) (App y z))
apply I x        = return x
apply V x        = return V
apply C x        = callCC (\c -> apply x (Cont c))
apply (Cont c) x = throw c x
apply D x        = return x
apply (D1 e) x   = do f <- eval e; apply f x
apply (Dot c) x  = step >> io (putChar c) >> return x
apply E x        = exit x

apply At f = do
  dat <- io $ catch (getChar >>= return . Just) (const $ return Nothing)
  setCurrentChar dat
  apply f (case dat of Nothing -> V ; Just _  -> I)

apply (Ques c) f = do
  cur <- currentChar
  apply f (if cur == Just c then I else V)

apply Pipe f = do
  cur <- currentChar
  apply f (case cur of Nothing -> V ; Just c  -> (Dot c))
