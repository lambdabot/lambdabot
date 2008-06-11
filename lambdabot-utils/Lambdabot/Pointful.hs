{-# OPTIONS -fno-warn-missing-signatures #-}
module Lambdabot.Pointful (pointful, ParseResult(..), test, main, combinatorModule) where

import Lambdabot.Parser

import Control.Monad.State
import Data.Generics
import Data.Maybe
import Language.Haskell.Parser
import Language.Haskell.Syntax
import qualified Data.Map as M

---- Utilities ----

extT' :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT' = extT
infixl `extT'`

unkLoc = SrcLoc "<new>" 1 1

stabilize f x = let x' = f x in if x' == x then x else stabilize f x'

namesIn h = everything (++) (mkQ [] (\x -> case x of UnQual name -> [name]; _ -> [])) h
pVarsIn h = everything (++) (mkQ [] (\x -> case x of HsPVar name -> [name]; _ -> [])) h

succName (HsIdent s) = HsIdent . reverse . succAlpha . reverse $ s

succAlpha ('z':xs) = 'a' : succAlpha xs
succAlpha (x  :xs) = succ x : xs
succAlpha []       = "a"

---- Optimization (removing explicit lambdas) and restoration of infix ops ----

-- move lambda patterns into LHS
optimizeD (HsPatBind loc (HsPVar fname) (HsUnGuardedRhs (HsLambda _ pats rhs)) [])
        =  HsFunBind [HsMatch loc fname pats (HsUnGuardedRhs rhs) []]
---- combine function binding and lambda
optimizeD (HsFunBind [HsMatch loc fname pats1 (HsUnGuardedRhs (HsLambda _ pats2 rhs)) []])
        =  HsFunBind [HsMatch loc fname (pats1 ++ pats2) (HsUnGuardedRhs rhs) []]
optimizeD x = x

-- remove parens
optimizeRhs (HsUnGuardedRhs (HsParen x))
          =  HsUnGuardedRhs x
optimizeRhs x = x

optimizeE :: HsExp -> HsExp
-- apply ((\x z -> ...x...) y) yielding (\z -> ...y...) if there is only one x or y is simple
optimizeE (HsApp (HsParen (HsLambda loc (HsPVar ident : pats) body)) arg) | single || simple
        = HsParen (HsLambda loc pats (everywhere (mkT (\x -> if x == (HsVar (UnQual ident)) then arg else x)) body))
  where single = gcount (mkQ False (== ident)) body == 1
        simple = case arg of HsVar _ -> True; _ -> False
-- apply ((\_ z -> ...) y) yielding (\z -> ...)
optimizeE (HsApp (HsParen (HsLambda loc (HsPWildCard : pats) body)) _)
        = HsParen (HsLambda loc pats body)
-- remove 0-arg lambdas resulting from application rules
optimizeE (HsLambda _ [] b)
        = b
-- replace (\x -> \y -> z) with (\x y -> z)
optimizeE (HsLambda loc p1 (HsLambda _ p2 body))
        = HsLambda loc (p1 ++ p2) body
-- remove double parens
optimizeE (HsParen (HsParen x))
        = HsParen x
-- remove lambda body parens
optimizeE (HsLambda l p (HsParen x))
        = HsLambda l p x
-- remove var, lit parens
optimizeE (HsParen x@(HsVar _))
        = x
optimizeE (HsParen x@(HsLit _))
        = x
-- remove infix+lambda parens
optimizeE (HsInfixApp a o (HsParen l@(HsLambda _ _ _)))
        = HsInfixApp a o l
-- remove left-assoc application parens
optimizeE (HsApp (HsParen (HsApp a b)) c)
        = HsApp (HsApp a b) c
-- restore infix
optimizeE (HsApp (HsApp (HsVar name@(UnQual (HsSymbol _))) l) r)
        = (HsInfixApp l (HsQVarOp name) r)
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
              (HsLambda _ ps _) -> do
                let pVars = concatMap pVarsIn ps
                newVars <- mapM (const fresh) pVars
                let replacements = zip pVars newVars
                return (everywhere (mkT (\n -> fromMaybe n (lookup n replacements))) e)
              _ -> return e))

uncomb' :: HsExp -> State (HsName, [HsName]) HsExp

-- expand plain combinators
uncomb' (HsVar qname) | isJust maybeDef = rename (fromJust maybeDef)
  where maybeDef = M.lookup qname combinators

-- eliminate sections
uncomb' (HsRightSection op arg)
  = do a <- fresh
       return (HsParen (HsLambda unkLoc [HsPVar a] (HsInfixApp (HsVar (UnQual a)) op arg)))
uncomb' (HsLeftSection arg op)
  = do a <- fresh
       return (HsParen (HsLambda unkLoc [HsPVar a] (HsInfixApp arg op (HsVar (UnQual a)))))
-- infix to prefix for canonicality
uncomb' (HsInfixApp lf (HsQVarOp name) rf)
  = return (HsParen (HsApp (HsApp (HsVar name) (HsParen lf)) (HsParen rf)))

-- fail
uncomb' expr = return expr

---- Simple combinator definitions ---

combinators = M.fromList $ map declToTuple defs
  where defs = case parseModule combinatorModule of
          ParseOk (HsModule _ _ _ _ d) -> d
          f@(ParseFailed _ _) -> error ("Combinator loading: " ++ show f)
        declToTuple (HsPatBind _ (HsPVar fname) (HsUnGuardedRhs body) [])
          = (UnQual fname, HsParen body)

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
  "                                                                     ",
  "-- ASSUMED reader monad                                              ",
  "-- (>>=)  = (\\f k r -> k (f r) r)                                   ",
  "-- return = const                                                    ",
  ""]

---- Top level ----

uncombOnce :: (Data a) => a -> a
uncombOnce x = evalState (everywhereM (mkM uncomb') x) (HsIdent "`", namesIn x ++ recognizedNames)
uncomb :: (Eq a, Data a) => a -> a
uncomb = stabilize uncombOnce

optimizeOnce :: (Data a) => a -> a
optimizeOnce x = everywhere (mkT optimizeD `extT'` optimizeRhs `extT'` optimizeE) x
optimize :: (Eq a, Data a) => a -> a
optimize = stabilize optimizeOnce

pointful = withParsed (optimize . uncomb)

test s = case parseModule s of
  f@(ParseFailed _ _) -> fail (show f)
  ParseOk (HsModule _ _ _ _ defs) ->
    flip mapM_ defs $ \def -> do
      putStrLn . prettyPrintInLine  $ def
      putStrLn . prettyPrintInLine  . uncomb $ def
      putStrLn . prettyPrintInLine  . optimize . uncomb $ def

main = test "f = tail . head; g = head . tail; h = tail + tail; three = g . h . i; dontSub = (\\x -> x + x) 1; ofHead f = f . head; fm = flip mapM_ xs (\\x -> g x); po = (+1); op = (1+); g = (. f); stabilize = fix (ap . flip (ap . (flip =<< (if' .) . (==))) =<<)"
