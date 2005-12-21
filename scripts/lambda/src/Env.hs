{-
 -   The Lambda Shell, and interactive environment for evaluating pure untyped lambda terms.
 -   Copyright (C) 2005, Robert Dockins
 -
 -   This program is free software; you can redistribute it and/or modify
 -   it under the terms of the GNU General Public License as published by
 -   the Free Software Foundation; either version 2 of the License, or
 -   (at your option) any later version.
 -
 -   This program is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU General Public License for more details.
 -
 -   You should have received a copy of the GNU General Public License
 -   along with this program; if not, write to the Free Software
 -   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 -}

module Env
( Env
, empty
, insert
, lookup
) where

import Prelude hiding (lookup)
import qualified Data.Set as Set

data Env = Env !Int ![String] !(Set.Set String)

empty :: Env
empty = Env 0 [] (Set.empty)

insert :: String -> Env -> Env
insert label (Env z labels set)
    | label `Set.member` set = Env (z+1) ( (label++"_"++(show z)) : labels) set
    | otherwise              = Env z     ( label : labels )                 (Set.insert label set)

lookup :: Int -> Env -> String

lookup x (Env z labels set) =
   case drop x labels of
      l:_ -> l
      []  -> error (concat ["'",show x,"' out of bounds in environment ",show labels])
