{-# OPTIONS -fno-warn-name-shadowing #-}
--
-- A wrapper over DrHylo, from the UMinho Haskell Software
-- DrHylo derives hylomorphisms from restricted Haskell syntax
--
-- The original DrHylo written by alcino@di.uminho.pt.
--
-- See also
--     http://wiki.di.uminho.pt/twiki/bin/view/Alcino/PointlessHaskell
--

module Plugin.DrHylo where

import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Control.Monad.State

import Plugin hiding (Module, Config(..))
import qualified Plugin as P (Module)

PLUGIN DrHylo

instance P.Module DrHyloModule () where

    process_ _ _ xs = io (hylo xs)

    moduleCmds _   = ["hylo"]
    moduleHelp _ _ = unlines 
       ["hylo <expr>. Derive hylomorphism for <expr>. Based on DrHylo."
       ,"Uses the Pointless.Combinators from:"
       ," http://wiki.di.uminho.pt/twiki/bin/view/Alcino/PointlessHaskell"
       ,"Mirrored:"
       ," http://www.cse.unsw.edu.au/~dons/Pointless/"]

hylo :: String -> IO [String]
hylo = ((drop 4 . lines . prettyPrint . dhModule) `fmap`) . parse

------------------------------------------------------------------------

-- I'm not collecting the global ids yet

mkHsImportDecl :: String -> HsImportDecl
mkHsImportDecl n = HsImportDecl mkLoc (Module n) False Nothing Nothing

dhModule :: HsModule -> HsModule
dhModule (HsModule loc name exports imports decls) =
    let decls' = map aux decls
        imports' = imports++[ mkHsImportDecl "Pointless.Combinators"
                            , mkHsImportDecl "Pointless.Functors"
                            , mkHsImportDecl "Pointless.RecursionPatterns"]
    in HsModule loc name exports imports' decls'
    where aux d = case (evalStateT (dhDecl d) initialSt)
                  of Just d' -> d'
                     Nothing -> d

dhDecl :: HsDecl -> ST HsDecl
dhDecl (HsFunBind matches) =
    do setNMatches (sum (map aux matches))
       sequence (map dMatch matches)
       functor <- gets functor
       cata <- gets cata
       ana <- gets ana
       name <- gets name
       return (mkHylo (fromJust name)
                    (HsTyApp (HsTyVar (mkName "Mu"))
                       (foldr1 mkSum functor)) ana cata)
    where aux (HsMatch _ _ _ (HsUnGuardedRhs _) _) = 1
          aux (HsMatch _ _ _ (HsGuardedRhss l) _) = length l
dhDecl _ = fail "Hylo derivation is only applied to functions"

mkHylo :: HsName -> HsType -> [HsMatch] -> [HsMatch] -> HsDecl
mkHylo name functor cata ana = 
    let rhs = HsUnGuardedRhs (HsApp (HsApp (HsApp (mkVar "hylo") (HsParen (HsExpTypeSig mkLoc (mkVar "_L") (HsQualType [] functor)))) (mkVar "g")) (mkVar "h"))
    in HsFunBind [(HsMatch mkLoc name [] rhs [HsFunBind ana, HsFunBind cata])]

dMatch :: HsMatch -> ST ()
dMatch (HsMatch _loc id pats (HsUnGuardedRhs rhs) wheres) = 
    do unless (length pats == 1 && null wheres) (fail "We only accept functions with one parameter and without wheres")
       setName id
       catarhs <- dExp rhs
       recs <- gets recs
       fvars <- gets fvars
       match <- gets match
       nmatches <- gets nmatches
       tyvar <- getFreshVar
       let functorrecs = map (\_ ->  mkId) recs
           catarecs = map (\(HsVar (UnQual v)) -> HsPVar v) (map fst recs)
           functorfvars = if (null fvars) then (if (null recs) then [mkConst (mkName "()")] else []) else [mkConst tyvar]
           anafvars = if (null fvars && not (null recs)) then [] else [HsTuple fvars]
           catafvars = if (null fvars && not (null recs)) then [] else [HsPTuple (map (\(HsVar (UnQual v)) -> HsPVar v) fvars)]
       addFunctor (foldr1 mkProd (functorfvars ++ functorrecs))
       addCata (HsMatch mkLoc (mkName "g") [mkPCons nmatches match (foldr1 mkPTuple (catafvars ++ catarecs))] (HsUnGuardedRhs catarhs) [])
       addAna (HsMatch mkLoc (mkName "h") pats (HsUnGuardedRhs (mkCons nmatches match (foldr1 mkTuple (anafvars ++ (map snd recs))))) [])
       nextMatch
       return ()
dMatch (HsMatch _loc id pats (HsGuardedRhss rhs) wheres) =
    do unless (length pats == 1 && null wheres) (fail "We only accept functions with one parameter and without wheres")
       setName id
       mapM (dGuardedRhs pats) rhs
       return ()

dGuardedRhs :: [HsPat] -> HsGuardedRhs -> ST ()
dGuardedRhs pats (HsGuardedRhs loc guard exp) =
    do catarhs <- dExp exp
       recs <- gets recs
       fvars <- gets fvars
       match <- gets match
       nmatches <- gets nmatches
       tyvar <- getFreshVar
       let functorrecs = map (\_ ->  mkId) recs
           catarecs = map (\(HsVar (UnQual v)) -> HsPVar v) (map fst recs)
           functorfvars = if (null fvars) then (if (null recs) then [mkConst (mkName "()")] else []) else [mkConst tyvar]
           anafvars = if (null fvars && not (null recs)) then [] else [HsTuple fvars]
           catafvars = if (null fvars && not (null recs)) then [] else [HsPTuple (map (\(HsVar (UnQual v)) -> HsPVar v) fvars)]
       addFunctor (foldr1 mkProd (functorfvars ++ functorrecs))
       addCata (HsMatch mkLoc (mkName "g") [mkPCons nmatches match (foldr1 mkPTuple (catafvars ++ catarecs))] (HsUnGuardedRhs catarhs) [])
       addAna (HsMatch mkLoc (mkName "h") pats (HsGuardedRhss [HsGuardedRhs loc guard (mkCons nmatches match (foldr1 mkTuple (anafvars ++ (map snd recs))))]) [])
       nextMatch
       return ()



-- Not exactly like in the paper: for the moment, I only consider the function to have 1 parameter
dExp :: HsExp -> ST HsExp
dExp (HsLit x) = return (HsLit x)
dExp (HsApp (HsVar (UnQual f)) e) = 
    do f' <- gets name
       if (f == (fromJust f'))
        then (do {u <- getFreshVar;
                  e' <- rLets e;
                  addRec ((HsVar (UnQual u)),e');
                  return (HsVar (UnQual u))})
        else (do {e' <- dExp e;
                  return (HsApp (HsVar (UnQual f)) e')})
dExp (HsApp (HsCon (UnQual c)) e) = 
    do e' <- dExp e
       return (HsApp (HsCon (UnQual c)) e')
dExp (HsApp a b) = 
    do a' <- dExp a
       b' <- dExp b
       return (HsApp a' b')
dExp (HsList l) =
    do l' <- mapM dExp l
       return (HsList l')
dExp (HsTuple l) =
    do l' <- mapM dExp l
       return (HsTuple l')
dExp (HsInfixApp l o r) = 
    do l' <- dExp l
       r' <- dExp r
       return (HsInfixApp l' o r')
-- there is an error in the paper concerning lets
dExp (HsLet [HsPatBind loc (HsPVar v) (HsUnGuardedRhs d) []] e) =
    do addLet (v,d)
       e' <- dExp e
       removeLet (v,d)
       d' <- dExp d
       return (HsLet [HsPatBind loc (HsPVar v) (HsUnGuardedRhs d') []] e')
dExp (HsVar (UnQual v)) = 
    do lets <- gets lets
       unless (v `elem` (map fst lets)) (addFVar (HsVar (UnQual v)))
       return (HsVar (UnQual v))
dExp (HsCon c) = return (HsCon c)
dExp (HsParen e) =
    do e' <- dExp e
       return (HsParen e')
dExp _ = fail "Can not handle all HsExps"


-- Replace lets

rLets :: HsExp -> ST HsExp
rLets (HsLit x) = return (HsLit x)
rLets (HsApp a b) = 
    do a' <- rLets a
       b' <- rLets b
       return (HsApp a' b')
rLets (HsList l) =
    do l' <- mapM rLets l
       return (HsList l')
rLets (HsTuple l) =
    do l' <- mapM rLets l
       return (HsTuple l')
rLets (HsInfixApp l o r) = 
    do l' <- rLets l
       r' <- rLets r
       return (HsInfixApp l' o r')
rLets (HsLeftSection l o) = 
    do l' <- rLets l
       return (HsLeftSection l' o)
rLets (HsRightSection o l) = 
    do l' <- rLets l
       return (HsRightSection o l')
rLets (HsVar (UnQual v)) = 
    do lets <- gets lets
       case (lookup v lets) 
            of Nothing -> do {return (HsVar (UnQual v))}
               Just e  -> do {return (HsParen (HsLet [HsPatBind mkLoc (HsPVar v) (HsUnGuardedRhs e) []] (HsVar (UnQual v))))}
rLets (HsCon c) = return (HsCon c)
rLets (HsParen e) =
    do e' <- rLets e
       return (HsParen e')
rLets _ = fail "Can not handle all HsExps"

-- Auxiliary definitions

mkSum :: HsType -> HsType -> HsType
mkSum l r = HsTyApp (HsTyApp l (HsTyVar (mkName ":+:"))) r

mkProd :: HsType -> HsType -> HsType
mkProd l r = HsTyApp (HsTyApp l (HsTyVar (mkName ":*:"))) r

mkId :: HsType
mkId = HsTyVar (mkName "Id")

mkConst :: HsName -> HsType
mkConst n = HsTyApp (HsTyVar (mkName "Const")) (HsTyVar n)

mkLoc :: SrcLoc
mkLoc = SrcLoc "" 0 0

mkVar :: String -> HsExp
mkVar s = HsVar (UnQual (mkName s))

mkName :: String -> HsName
mkName s = (HsIdent s)

mkTuple :: HsExp -> HsExp -> HsExp
mkTuple l r = HsTuple [l,r]

mkPTuple :: HsPat -> HsPat -> HsPat
mkPTuple l r = HsPTuple [l,r]

mkCons :: Int -> Int -> HsExp -> HsExp
mkCons 1 0 e = e
mkCons m n e = 
    let aux = (replicate n (HsCon (UnQual (mkName "Right")))) ++ (if (n==(m-1)) then [] else [HsCon (UnQual (mkName "Left"))])
    in foldr1 (\l r -> HsApp l (HsParen r)) (aux++[e])

mkPCons :: Int -> Int -> HsPat -> HsPat
mkPCons 1 0 e = e
mkPCons m n e = 
    let aux = (replicate n (HsPVar (mkName "Right"))) ++ (if (n==(m-1)) then [] else [HsPVar (mkName "Left")])
    in HsPParen (foldr1 (\(HsPVar l) r -> HsPApp (UnQual l) [(HsPParen r)]) (aux++[e]))

-- The State of our Monad

type ST = StateT St Maybe

data St = St {lets :: [(HsName,HsExp)], 
              name :: Maybe HsName,
              seed :: Int,
              fvars :: [HsExp],
              recs :: [(HsExp,HsExp)],
              ana :: [HsMatch],
              cata :: [HsMatch],
              functor :: [HsType],
              match :: Int,
              nmatches :: Int
             }

initialSt :: St
initialSt = St {lets = [], 
                name = Nothing,
                seed = 0,
                fvars = [],
                recs = [],
                cata = [],
                ana = [],
                functor = [],
                match = 0,
                nmatches = 0
               }

setName :: HsName -> ST ()
setName n = modify (\s -> s {name = Just n})

nextMatch :: ST ()
nextMatch = modify (\s -> s {fvars = [], recs = [], match = match s + 1})

setNMatches :: Int -> ST ()
setNMatches n = modify (\s -> s {nmatches = n})

getSeed :: ST Int
getSeed = do n <- gets seed
             modify (\s -> s {seed = n+1})
             return n

getFreshVar :: ST HsName
getFreshVar = do s <- getSeed
                 return (HsIdent ("v"++(show s)))

addLet :: (HsName,HsExp) -> ST ()
addLet v = modify (\s -> s {lets = v:(lets s)})

removeLet :: (HsName,HsExp) -> ST ()
removeLet v = modify (\s -> s {lets = delete v (lets s)})

addFVar :: HsExp -> ST ()
addFVar v = modify (\s -> s {fvars = fvars s ++ [v]})

addRec :: (HsExp,HsExp) -> ST ()
addRec r = modify (\s -> s {recs = recs s ++ [r]})

addCata :: HsMatch -> ST ()
addCata v = modify (\s -> s {cata = cata s ++ [v]})

addAna :: HsMatch -> ST ()
addAna v = modify (\s -> s {ana = ana s ++ [v]})

addFunctor :: HsType -> ST ()
addFunctor v = modify (\s -> s {functor = functor s ++ [v]})

------------------------------------------------------------------------

parse :: String -> IO HsModule
parse s = case (parseModule s)
          of ParseOk m -> return m
             ParseFailed l d -> fail ((show l)++": "++d)

{-

plus :: (Int, Int) -> Int
plus = uncurry (+)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert (x,(isort xs))

insert :: Ord a => (a,[a]) -> [a]
insert (x,[]) = [x]
insert (x,y:ys) | x<=y = x:y:ys
                | otherwise = y:(insert (x,ys))

fact 0 = 1
fact n = n * fact (n-1)

len [] = 0
len (x:xs) = 1+(len xs)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = let aux = msplit l
          in merge (msort (fst aux), msort (snd aux))

msplit :: [a] -> ([a],[a])
msplit [] = ([],[])
msplit (x:xs) = let aux = msplit xs
                in (x:snd aux, fst aux)

merge :: (Ord a) => ([a],[a]) -> [a]
merge ([],l) = l
merge (l,[]) = l
merge (x:xs,y:ys) | x<=y = x:merge (xs,y:ys)
                  | otherwise = y:merge (x:xs,ys)

hsort :: (Ord a) => [a] -> [a]
hsort [] = []
hsort l = let aux = hsplit l
           in (fst aux):(merge (hsort (fst (snd aux)), hsort (snd (snd aux))))

hsplit :: (Ord a) => [a] -> (a,([a],[a]))
hsplit [x] = (x,([],[]))
hsplit (h:t) | h < m     = (h,(m:l,r))
             | otherwise = (m,(h:r,l))
             where (m,(l,r)) = hsplit t

-}
