{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- Undo pointfree transformations. Plugin code derived from Pl.hs.
module Lambdabot.Plugin.Haskell.Pointful (pointfulPlugin) where

import Lambdabot.Module as Lmb (Module)
import Lambdabot.Plugin
import Lambdabot.Util.Parser (withParsed, prettyPrintInLine)

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity (Identity)
import Data.Generics
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Language.Haskell.Exts as Hs

pointfulPlugin :: Lmb.Module ()
pointfulPlugin = newModule
    { moduleCmds = return
        [ (command "pointful")
            { aliases = ["pointy","repoint","unpointless","unpl","unpf"]
            , help = say "pointful <expr>. Make code pointier."
            , process = mapM_ say . lines . pointful
            }
        ]
    }

---- Utilities ----

unkLoc :: SrcLoc
unkLoc = SrcLoc "<new>" 1 1

stabilize :: Eq a => (a -> a) -> a -> a
stabilize f x = let x' = f x in if x' == x then x else stabilize f x'

-- varsBoundHere returns variables bound by top patterns or binders
varsBoundHere :: Data d => d -> S.Set Name
varsBoundHere (cast -> Just (PVar name)) = S.singleton name
varsBoundHere (cast -> Just (Match _ name _ _ _ _)) = S.singleton name
varsBoundHere (cast -> Just (PatBind _ pat _ _)) = varsBoundHere pat
varsBoundHere (cast -> Just (_ :: Exp)) = S.empty
varsBoundHere d = S.unions (gmapQ varsBoundHere d)

-- note: the tempting idea of using a pattern synonym for the frequent
-- (cast -> Just _) patterns causes compiler crashes with ghc before
-- version 8; cf. https://ghc.haskell.org/trac/ghc/ticket/11336

foldFreeVars :: forall a d. Data d => (Name -> S.Set Name -> a) -> ([a] -> a) -> d -> a
foldFreeVars var sum e = runReader (go e) S.empty where
    go :: forall d. Data d => d -> Reader (S.Set Name) a
    go (cast -> Just (Var (UnQual name))) =
        asks (var name)
    go (cast -> Just (Lambda _ ps exp)) =
        bind [varsBoundHere ps] $ go exp
    go (cast -> Just (Let bs exp)) =
        bind [varsBoundHere bs] $ collect [go bs, go exp]
    go (cast -> Just (Alt _ pat exp bs)) =
        bind [varsBoundHere pat, varsBoundHere bs] $ collect [go exp, go bs]
    go (cast -> Just (PatBind _ pat exp bs)) =
        bind [varsBoundHere pat, varsBoundHere bs] $ collect [go exp, go bs]
    go (cast -> Just (Match _ _ ps _ exp bs)) =
        bind [varsBoundHere ps, varsBoundHere bs] $ collect [go exp, go bs]
    go d = collect (gmapQ go d)

    collect :: forall m. Monad m => [m a] -> m a
    collect ms = sum `liftM` sequence ms

    bind :: forall a b. Ord a => [S.Set a] -> Reader (S.Set a) b -> Reader (S.Set a) b
    bind ss = local (S.unions ss `S.union`)

-- return free variables
freeVars :: Data d => d -> S.Set Name
freeVars = foldFreeVars (\name bv -> S.singleton name `S.difference` bv) S.unions

-- return number of free occurrences of a variable
countOcc :: Data d => Name -> d -> Int
countOcc name = foldFreeVars var sum where
    sum = foldl' (+) 0
    var name' bv = if name /= name' || name' `S.member` bv then 0 else 1

-- variable capture avoiding substitution
substAvoiding :: Data d => M.Map Name Exp -> S.Set Name -> d -> d
substAvoiding subst bv = base `extT` exp `extT` alt `extT` decl `extT` match where
    base :: Data d => d -> d
    base = gmapT (substAvoiding subst bv)

    exp e@(Var (UnQual name)) =
        fromMaybe e (M.lookup name subst)
    exp (Lambda sloc ps exp) =
        let (subst', bv', ps') = renameBinds subst bv ps
        in  Lambda sloc ps' (substAvoiding subst' bv' exp)
    exp (Let bs exp) =
        let (subst', bv', bs') = renameBinds subst bv bs
        in  Let (substAvoiding subst' bv' bs') (substAvoiding subst' bv' exp)
    exp d = base d

    alt (Alt sloc pat exp bs) =
        let (subst1, bv1, pat') = renameBinds subst bv pat
            (subst', bv', bs') = renameBinds subst1 bv1 bs
        in  Alt sloc pat' (substAvoiding subst' bv' exp) (substAvoiding subst' bv' bs')

    decl (PatBind sloc pat exp bs) =
        let (subst', bv', bs') = renameBinds subst bv bs in
        PatBind sloc pat (substAvoiding subst' bv' exp) (substAvoiding subst' bv' bs')
    decl d = base d

    match (Match sloc name ps typ exp bs) =
        let (subst1, bv1, ps') = renameBinds subst bv ps
            (subst', bv', bs') = renameBinds subst1 bv1 bs
        in  Match sloc name ps' typ (substAvoiding subst' bv' exp) (substAvoiding subst' bv' bs')

-- rename local binders (but not the nested expressions)
renameBinds :: Data d => M.Map Name Exp -> S.Set Name -> d -> (M.Map Name Exp, S.Set Name, d)
renameBinds subst bv d = (subst', bv', d') where
    (d', (subst', bv', _)) = runState (go d) (subst, bv, M.empty)

    go, base :: Data d => d -> State (M.Map Name Exp, S.Set Name, M.Map Name Name) d
    go = base `extM` pat `extM` match `extM` decl `extM` exp
    base d = gmapM go d

    pat (PVar name) = PVar `fmap` rename name
    pat d = base d

    match (Match sloc name ps typ exp bs) = do
        name' <- rename name
        return $ Match sloc name' ps typ exp bs

    decl (PatBind sloc pat exp bs) = do
        pat' <- go pat
        return $ PatBind sloc pat' exp bs
    decl d = base d

    exp (e :: Exp) = return e

    rename :: Name -> State (M.Map Name Exp, S.Set Name, M.Map Name Name) Name
    rename name = do
        (subst, bv, ass) <- get
        case (name `M.lookup` ass, name `S.member` bv) of
            (Just name', _) -> do
                 return name'
            (_, False) -> do
                put (M.delete name subst, S.insert name bv, ass)
                return name
            _ -> do
                let name' = freshNameAvoiding name bv
                put (M.insert name (Var (UnQual name')) subst,
                     S.insert name' bv, M.insert name name' ass)
                return name'

-- generate fresh names
freshNameAvoiding :: Name -> S.Set Name -> Name
freshNameAvoiding name forbidden  = con (pre ++ suf) where
    (con, nm, cs) = case name of
         Ident  n -> (Ident,  n, "0123456789")
         Symbol n -> (Symbol, n, "?#")
    pre = reverse . dropWhile (`elem` cs) . reverse $ nm
    sufs = [1..] >>= flip replicateM cs
    suf = head $ dropWhile (\suf -> con (pre ++ suf) `S.member` forbidden) sufs

---- Optimization (removing explicit lambdas) and restoration of infix ops ----

-- move lambda patterns into LHS
optimizeD :: Decl -> Decl
optimizeD (PatBind locat (PVar fname) (UnGuardedRhs (Lambda _ pats rhs)) Nothing) =
    let (subst, bv, pats') = renameBinds M.empty (S.singleton fname) pats
        rhs' = substAvoiding subst bv rhs
    in  FunBind [Match locat fname pats' Nothing (UnGuardedRhs rhs') Nothing]
---- combine function binding and lambda
optimizeD (FunBind [Match locat fname pats1 Nothing (UnGuardedRhs (Lambda _ pats2 rhs)) Nothing]) =
    let (subst, bv, pats2') = renameBinds M.empty (varsBoundHere pats1) pats2
        rhs' = substAvoiding subst bv rhs
    in  FunBind [Match locat fname (pats1 ++ pats2') Nothing (UnGuardedRhs rhs') Nothing]
optimizeD x = x

-- remove parens
optimizeRhs :: Rhs -> Rhs
optimizeRhs (UnGuardedRhs (Paren x)) = UnGuardedRhs x
optimizeRhs x = x

optimizeE :: Exp -> Exp
-- apply ((\x z -> ...x...) y) yielding (\z -> ...y...) if there is only one x or y is simple
optimizeE (App (Lambda locat (PVar ident : pats) body) arg) | single || simple arg =
     let (subst, bv, pats') = renameBinds (M.singleton ident arg) (freeVars arg) pats
     in  Paren (Lambda locat pats' (substAvoiding subst bv body))
  where
    single = countOcc ident body <= 1
    simple e = case e of Var _ -> True; Lit _ -> True; Paren e' -> simple e'; _ -> False
-- apply ((\_ z -> ...) y) yielding (\z -> ...)
optimizeE (App (Lambda locat (PWildCard : pats) body) _) =
    Paren (Lambda locat pats body)
-- remove 0-arg lambdas resulting from application rules
optimizeE (Lambda _ [] b) =
    b
-- replace (\x -> \y -> z) with (\x y -> z)
optimizeE (Lambda locat p1 (Lambda _ p2 body)) =
    let (subst, bv, p2') = renameBinds M.empty (varsBoundHere p1) p2
        body' = substAvoiding subst bv body
    in  Lambda locat (p1 ++ p2') body'
-- remove double parens
optimizeE (Paren (Paren x)) =
    Paren x
-- remove parens around applied lambdas (the pretty printer restores them)
optimizeE (App (Paren (x@Lambda{})) y) =
    App x y
-- remove lambda body parens
optimizeE (Lambda l p (Paren x)) =
    Lambda l p x
-- remove var, lit parens
optimizeE (Paren x@(Var _)) =
    x
optimizeE (Paren x@(Lit _)) =
    x
-- remove infix+lambda parens
optimizeE (InfixApp a o (Paren l@(Lambda _ _ _))) =
    InfixApp a o l
-- remove infix+app aprens
optimizeE (InfixApp (Paren a@App{}) o l) =
    InfixApp a o l
optimizeE (InfixApp a o (Paren l@App{})) =
    InfixApp a o l
-- remove left-assoc application parens
optimizeE (App (Paren (App a b)) c) =
    App (App a b) c
-- restore infix
optimizeE (App (App (Var name'@(UnQual (Symbol _))) l) r) =
    (InfixApp l (QVarOp name') r)
-- eta reduce
optimizeE (Lambda l ps@(_:_) (App e (Var (UnQual v))))
    | free && last ps == PVar v = Lambda l (init ps) e
  where free = countOcc v e == 0
-- fail
optimizeE x = x

---- Decombinatorization ----

uncomb' :: Exp -> Exp

uncomb' (Paren (Paren e)) = Paren e

-- eliminate sections
uncomb' (RightSection op' arg) =
    let a = freshNameAvoiding (Ident "a") (freeVars arg)
    in  (Paren (Lambda unkLoc [PVar a] (InfixApp (Var (UnQual a)) op' arg)))
uncomb' (LeftSection arg op') =
    let a = freshNameAvoiding (Ident "a") (freeVars arg)
    in  (Paren (Lambda unkLoc [PVar a] (InfixApp arg op' (Var (UnQual a)))))
-- infix to prefix for canonicality
uncomb' (InfixApp lf (QVarOp name') rf) =
    (Paren (App (App (Var name') (Paren lf)) (Paren rf)))

-- Expand (>>=) when it is obviously the reader monad:

-- rewrite: (>>=) (\x -> e)
-- to:      (\ a b -> a ((\ x -> e) b) b)
uncomb' (App (Var (UnQual (Symbol ">>="))) (Paren lam@Lambda{})) =
   let a = freshNameAvoiding (Ident "a") (freeVars lam)
       b = freshNameAvoiding (Ident "b") (freeVars lam)
   in  (Paren (Lambda unkLoc [PVar a, PVar b]
           (App (App (Var (UnQual a)) (Paren (App lam (Var (UnQual b))))) (Var (UnQual b)))))
-- rewrite: ((>>=) e1) (\x y -> e2)
-- to:      (\a -> (\x y -> e2) (e1 a) a)
uncomb' (App (App (Var (UnQual (Symbol ">>="))) e1) (Paren lam@(Lambda _ (_:_:_) _))) =
    let a = freshNameAvoiding (Ident "a") (freeVars [e1,lam])
    in  (Paren (Lambda unkLoc [PVar a]
            (App (App lam (App e1 (Var (UnQual a)))) (Var (UnQual a)))))

-- fail
uncomb' expr = expr

---- Simple combinator definitions ---
combinators :: M.Map Name Exp
combinators = M.fromList $ map declToTuple defs
  where defs = case parseModule combinatorModule of
          ParseOk (Hs.Module _ _ _ _ _ _ d) -> d
          f@(ParseFailed _ _) -> error ("Combinator loading: " ++ show f)
        declToTuple (PatBind _ (PVar fname) (UnGuardedRhs body) Nothing)
          = (fname, Paren body)
        declToTuple _ = error "Pointful Plugin error: can't convert declaration to tuple"

combinatorModule :: String
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

unfoldCombinators :: (Data a) => a -> a
unfoldCombinators = substAvoiding combinators (freeVars combinators)

uncombOnce :: (Data a) => a -> a
uncombOnce x = everywhere (mkT uncomb') x
uncomb :: (Eq a, Data a) => a -> a
uncomb = stabilize uncombOnce

optimizeOnce :: (Data a) => a -> a
optimizeOnce x = everywhere (mkT optimizeD `extT` optimizeRhs `extT` optimizeE) x
optimize :: (Eq a, Data a) => a -> a
optimize = stabilize optimizeOnce

pointful :: String -> String
pointful = withParsed (stabilize (optimize . uncomb) . stabilize (unfoldCombinators . uncomb))

-- TODO: merge this into a proper test suite once one exists
-- test s = case parseModule s of
--   f@(ParseFailed _ _) -> fail (show f)
--   ParseOk (Hs.Module _ _ _ _ _ _ defs) ->
--     flip mapM_ defs $ \def -> do
--       putStrLn . prettyPrintInLine  $ def
--       putStrLn . prettyPrintInLine  . uncomb $ def
--       putStrLn . prettyPrintInLine  . optimize . uncomb $ def
--       putStrLn . prettyPrintInLine  . stabilize (optimize . uncomb) $ def
--       putStrLn ""
--
-- main = test "f = tail . head; g = head . tail; h = tail + tail; three = g . h . i; dontSub = (\\x -> x + x) 1; ofHead f = f . head; fm = flip mapM_ xs (\\x -> g x); po = (+1); op = (1+); g = (. f); stabilize = fix (ap . flip (ap . (flip =<< (if' .) . (==))) =<<)"
--
