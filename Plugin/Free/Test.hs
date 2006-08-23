module Plugin.Free.Test where

import Plugin.Free.FreeTheorem
import Plugin.Free.Type


tUndef = "undefined :: a -> a"

tMzero = "mzero :: [a]"

tReturnList = "return :: a -> [a]"

tHead = "head :: [a] -> a"

tTail = "tail :: [a] -> [a]"

tId = "id :: a -> a"

tConst = "const :: a -> b -> a"

tIdPair = "id :: (a,b) -> (a,b)"

tSwap = "swap :: (a,b) -> (b,a)"

tGenSwap = "genSwap :: (forall z. a -> b -> z) -> (forall z. b -> a -> z)"

tMap = "map :: (a -> b) -> ([a] -> [b])"

tZip = "zip :: ([a],[b]) -> [(a,b)]"

tIdFun = "id :: (a -> b) -> (a -> b)"

tFst = "fst :: (a,b) -> a"

tFstFun = "fst :: (a->b,c) -> a -> b"

tSnd = "snd :: (a,b) -> b"


tContinuation :: Type -> Type
tContinuation a
    = TyForall "R" (TyArr (TyArr a r) r)
    where
        r = TyVar "R"

tReturnC = "return :: a -> (forall r. (a -> r) -> r)"

tCallCC = "callcc :: ((a -> (forall r. (b -> r) -> r)) -> (forall r. (a -> r) -> r)) -> (forall r. (a -> r) -> r)"

tPierce = "pierce :: ((a -> b) -> a) -> a"

tNot = "not :: (forall z. z -> z -> z) -> (forall z. z -> z -> z)"

-- vim: ts=4:sts=4:expandtab:ai
