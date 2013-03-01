{-# OPTIONS -fno-warn-missing-signatures #-}
module Lambdabot.Util.Pointful (pointful, ParseResult(..), test, main, combinatorModule) where

import Lambdabot.Util.Parser (withParsed, prettyPrintInLine)

import Control.Monad.State
import Data.Generics
import Data.Maybe
import Language.Haskell.Exts
import qualified Data.Map as M

---- Utilities ----

extT' :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT' = extT
infixl `extT'`

unkLoc = SrcLoc "<new>" 1 1

stabilize f x = let x' = f x in if x' == x then x else stabilize f x'

namesIn h = everything (++) (mkQ [] (\x -> case x of UnQual name -> [name]; _ -> [])) h
pVarsIn h = everything (++) (mkQ [] (\x -> case x of PVar name -> [name]; _ -> [])) h

succName (Ident s) = Ident . reverse . succAlpha . reverse $ s

succAlpha ('z':xs) = 'a' : succAlpha xs
succAlpha (x  :xs) = succ x : xs
succAlpha []       = "a"

---- Optimization (removing explicit lambdas) and restoration of infix ops ----

-- move lambda patterns into LHS
optimizeD (PatBind loc (PVar fname) Nothing (UnGuardedRhs (Lambda _ pats rhs)) (BDecls []))
        =  FunBind [Match loc fname pats Nothing (UnGuardedRhs rhs) (BDecls [])]
---- combine function binding and lambda
optimizeD (FunBind [Match loc fname pats1 Nothing (UnGuardedRhs (Lambda _ pats2 rhs)) (BDecls [])])
        =  FunBind [Match loc fname (pats1 ++ pats2) Nothing (UnGuardedRhs rhs) (BDecls [])]
optimizeD x = x

-- remove parens
optimizeRhs (UnGuardedRhs (Paren x))
          =  UnGuardedRhs x
optimizeRhs x = x

optimizeE :: Exp -> Exp
-- apply ((\x z -> ...x...) y) yielding (\z -> ...y...) if there is only one x or y is simple
  -- TODO: avoid captures while substituting
optimizeE (App (Paren (Lambda loc (PVar ident : pats) body)) arg) | single || simple arg
        = Paren (Lambda loc pats (everywhere (mkT (\x -> if x == (Var (UnQual ident)) then arg else x)) body))
  where single = gcount (mkQ False (== ident)) body <= 1
        simple e = case e of Var _ -> True; Lit _ -> True; Paren e -> simple e; _ -> False
-- apply ((\_ z -> ...) y) yielding (\z -> ...)
optimizeE (App (Paren (Lambda loc (PWildCard : pats) body)) _)
        = Paren (Lambda loc pats body)
-- remove 0-arg lambdas resulting from application rules
optimizeE (Lambda _ [] b)
        = b
-- replace (\x -> \y -> z) with (\x y -> z)
optimizeE (Lambda loc p1 (Lambda _ p2 body))
        = Lambda loc (p1 ++ p2) body
-- remove double parens
optimizeE (Paren (Paren x))
        = Paren x
-- remove lambda body parens
optimizeE (Lambda l p (Paren x))
        = Lambda l p x
-- remove var, lit parens
optimizeE (Paren x@(Var _))
        = x
optimizeE (Paren x@(Lit _))
        = x
-- remove infix+lambda parens
optimizeE (InfixApp a o (Paren l@(Lambda _ _ _)))
        = InfixApp a o l
-- remove left-assoc application parens
optimizeE (App (Paren (App a b)) c)
        = App (App a b) c
-- restore infix
optimizeE (App (App (Var name@(UnQual (Symbol _))) l) r)
        = (InfixApp l (QVarOp name) r)
-- eta reduce
optimizeE (Lambda l ps@(_:_) (App e (Var (UnQual v))))
  | free && last ps == PVar v
        = Lambda l (init ps) e
  where free = gcount (mkQ False (== v)) e == 0
        simple e = case e of Var _ -> True; Lit _ -> True; Paren e -> simple e; _ -> False
-- fail
optimizeE x = x

---- Decombinatorization ----

-- fresh name generation. TODO: prettify this
fresh = do (_,    used) <- get
           modify (\(v,u) -> (until (not . (`elem` used)) succName (succName v), u))
           (name, _) <- get
           return name

-- rename all lambda-bound variables. TODO: rewrite lets as well
rename = do everywhereM (mkM (\e -> case e of
              (Lambda _ ps _) -> do
                let pVars = concatMap pVarsIn ps
                newVars <- mapM (const fresh) pVars
                let replacements = zip pVars newVars
                return (everywhere (mkT (\n -> fromMaybe n (lookup n replacements))) e)
              _ -> return e))

uncomb' :: Exp -> State (Name, [Name]) Exp

uncomb' (Paren (Paren e)) = return (Paren e)

-- expand plain combinators
uncomb' (Var qname) | isJust maybeDef = rename (fromJust maybeDef)
  where maybeDef = M.lookup qname combinators

-- eliminate sections
uncomb' (RightSection op arg)
  = do a <- fresh
       return (Paren (Lambda unkLoc [PVar a] (InfixApp (Var (UnQual a)) op arg)))
uncomb' (LeftSection arg op)
  = do a <- fresh
       return (Paren (Lambda unkLoc [PVar a] (InfixApp arg op (Var (UnQual a)))))
-- infix to prefix for canonicality
uncomb' (InfixApp lf (QVarOp name) rf)
  = return (Paren (App (App (Var name) (Paren lf)) (Paren rf)))

-- Expand (>>=) when it is obviously the reader monad:

-- rewrite: (>>=) (\x -> e)
-- to:      (\ a b -> a ((\ x -> e) b) b)
uncomb' (App (Var (UnQual (Symbol ">>="))) (Paren lam@Lambda{}))
  = do a <- fresh
       b <- fresh
       return (Paren (Lambda unkLoc [PVar a, PVar b] 
                 (App (App (Var (UnQual a)) (Paren (App lam (Var (UnQual b))))) (Var (UnQual b)))))
-- rewrite: ((>>=) e1) (\x y -> e2)
-- to:      (\a -> (\x y -> e2) (e1 a) a) 
uncomb' (App (App (Var (UnQual (Symbol ">>="))) e1) (Paren lam@(Lambda _ (_:_:_) _)))
  = do a <- fresh
       return (Paren (Lambda unkLoc [PVar a]
                (App (App lam (App e1 (Var (UnQual a)))) (Var (UnQual a)))))

-- fail
uncomb' expr = return expr

---- Simple combinator definitions ---

combinators = M.fromList $ map declToTuple defs
  where defs = case parseModule combinatorModule of
          ParseOk (Module _ _ _ _ _ _ d) -> d
          f@(ParseFailed _ _) -> error ("Combinator loading: " ++ show f)
        declToTuple (PatBind _ (PVar fname) _ (UnGuardedRhs body) (BDecls []))
          = (UnQual fname, Paren body)

-- the names we recognize as combinators, so we don't generate them as temporaries then substitute them.
-- TODO: more generally correct would be to not substitute any variable which is bound by a pattern
recognizedNames = map (\(UnQual n) -> n) $ M.keys combinators

combinatorModule = unlines [
  "(.)    = \\f g x -> f (g x)                                          ",
  "($)    = \\f x   -> f x                                              ",
  "flip   = \\f x y -> f y x                                            ",
  "const  = \\x _ -> x                                                  ",
  "id     = \\x -> x                                                    ",
  "(=<<)  = flip (>>=)                                                  ",
  "liftM2 = \\f m1 m2 -> m1 >>= \\x1 -> m2 >>= \\x2 -> return (f x1 x2) ",
  "join   = (>>= id)                                                    ",
  "ap     = liftM2 id                                                   ",
  "(>=>)  = flip (<=<)                                                  ",
  "(<=<)  = \\f g x -> f >>= g x                                        ",
  "                                                                     ",
  "-- ASSUMED reader monad                                              ",
  "-- (>>=)  = (\\f k r -> k (f r) r)                                   ",
  "-- return = const                                                    ",
  ""]

---- Top level ----

uncombOnce :: (Data a) => a -> a
uncombOnce x = evalState (everywhereM (mkM uncomb') x) (Ident "`", namesIn x ++ recognizedNames)
uncomb :: (Eq a, Data a) => a -> a
uncomb = stabilize uncombOnce

optimizeOnce :: (Data a) => a -> a
optimizeOnce x = everywhere (mkT optimizeD `extT'` optimizeRhs `extT'` optimizeE) x
optimize :: (Eq a, Data a) => a -> a
optimize = stabilize optimizeOnce

pointful = withParsed (stabilize (optimize . uncomb))

test s = case parseModule s of
  f@(ParseFailed _ _) -> fail (show f)
  ParseOk (Module _ _ _ _ _ _ defs) ->
    flip mapM_ defs $ \def -> do
      putStrLn . prettyPrintInLine  $ def
      putStrLn . prettyPrintInLine  . uncomb $ def
      putStrLn . prettyPrintInLine  . optimize . uncomb $ def
      putStrLn . prettyPrintInLine  . stabilize (optimize . uncomb) $ def
      putStrLn ""

main = test "f = tail . head; g = head . tail; h = tail + tail; three = g . h . i; dontSub = (\\x -> x + x) 1; ofHead f = f . head; fm = flip mapM_ xs (\\x -> g x); po = (+1); op = (1+); g = (. f); stabilize = fix (ap . flip (ap . (flip =<< (if' .) . (==))) =<<)"
