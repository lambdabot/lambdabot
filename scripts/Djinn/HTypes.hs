module HTypes(HType(..), HSymbol, hTypeToFormula, pHSymbol, pHType, pHDataType, htNot, isHTUnion,
	HClause, HPat, HExpr, hPrClause, termToHExpr, termToHClause, getBinderVars) where
import Text.PrettyPrint.HughesPJ(Doc, renderStyle, style, text, (<>), parens, ($$), vcat, punctuate,
	 sep, fsep, nest, comma, (<+>))
import Char(isAlphaNum, isAlpha, isUpper)
import Text.ParserCombinators.ReadP
import LJTFormula

--import Debug.Trace

type HSymbol = String

data HType
	= HTApp HType HType
	| HTVar HSymbol
	| HTCon HSymbol
	| HTTuple [HType]
	| HTArrow HType HType
	| HTUnion [(HSymbol, [HType])]		-- Only for data types; only at top level
	deriving (Eq)

isHTUnion :: HType -> Bool
isHTUnion (HTUnion _) = True
isHTUnion _ = False

htNot :: HSymbol -> HType
htNot x = HTArrow (HTVar x) (HTCon "Void")

instance Show HType where
    showsPrec _ (HTApp (HTCon "[]") t) = showString "[" . showsPrec 0 t . showString "]"
    showsPrec p (HTApp f a) = showParen (p > 2) $ showsPrec 2 f . showString " " . showsPrec 3 a
    showsPrec _ (HTVar s) = showString s
    showsPrec _ (HTCon s) = showString s
    showsPrec _ (HTTuple ss) = showParen True $ f ss
	where f [] = error "showsPrec HType"
	      f [t] = showsPrec 0 t
	      f (t:ts) = showsPrec 0 t . showString ", " . f ts
    showsPrec p (HTArrow s t) = showParen (p > 0) $ showsPrec 1 s . showString " -> " . showsPrec 0 t
    showsPrec _ (HTUnion cs) = f cs
	where f [] = id
	      f [cts] = scts cts
	      f (cts : ctss) = scts cts . showString " | " . f ctss
	      scts (c, ts) = foldl (\ s t -> s . showString " " . showsPrec 10 t) (showString c) ts

instance Read HType where
    readsPrec _ = readP_to_S pHType'

pHType' :: ReadP HType
pHType' = do
    t <- pHType
    skipSpaces
    return t

pHType :: ReadP HType
pHType = do
    ts <- sepBy1 pHTypeApp (do schar '-'; char '>')
    return $ foldr1 HTArrow ts

pHDataType :: ReadP HType
pHDataType = do
    let con = do
	    c <- pHSymbol True
	    ts <- many pHTAtom
	    return (c, ts)
    cts <- sepBy con (schar '|')
    return $ HTUnion cts

pHTAtom :: ReadP HType
pHTAtom = pHTVar +++ pHTCon +++ pHTList +++ pParen pHTTuple +++ pParen pHType +++ pUnit

pUnit :: ReadP HType
pUnit = do
    schar '('
    char ')'
    return $ HTCon "()"

pHTCon :: ReadP HType
pHTCon = pHSymbol True >>= return . HTCon

pHTVar :: ReadP HType
pHTVar = pHSymbol False >>= return . HTVar

pHSymbol :: Bool -> ReadP HSymbol
pHSymbol con = do
    skipSpaces
    c <- satisfy $ \ c -> isAlpha c && isUpper c == con
    let isSym d = isAlphaNum d || d == '\'' || d == '.'
    cs <- munch isSym
    return $ c:cs

pHTTuple :: ReadP HType
pHTTuple = do
    t <- pHType
    ts <- many1 (do schar ','; pHType)
    return $ HTTuple $ t:ts

pHTypeApp :: ReadP HType
pHTypeApp = do
    ts <- many1 pHTAtom
    return $ foldl1 HTApp ts

pHTList :: ReadP HType
pHTList = do
    schar '['
    t <- pHType
    schar ']'
    return $ HTApp (HTCon "[]") t

pParen :: ReadP a -> ReadP a
pParen p = do
    schar '('
    e <- p
    schar ')'
    return e

schar :: Char -> ReadP ()
schar c = do
    skipSpaces
    char c
    return ()



-------------------------------

hTypeToFormula :: [(HSymbol, ([HSymbol], HType))] -> HType -> Formula
hTypeToFormula ss (HTTuple ts) = Conj (map (hTypeToFormula ss) ts)
hTypeToFormula ss (HTArrow t1 t2) = hTypeToFormula ss t1 :-> hTypeToFormula ss t2
hTypeToFormula ss (HTUnion ctss) = Disj [ (ConsDesc c (length ts), hTypeToFormula ss (HTTuple ts)) | (c, ts) <- ctss ]
hTypeToFormula ss t = 
    case expandSyn ss t [] of
    Nothing -> PVar $ Symbol $ show t
    Just t' -> hTypeToFormula ss t'

expandSyn :: [(HSymbol, ([HSymbol], HType))] -> HType -> [HType] -> Maybe HType
expandSyn ss (HTApp f a) as = expandSyn ss f (a:as)
expandSyn ss (HTCon c) as =
    case lookup c ss of
    Just (vs, t) | length vs == length as -> Just $ substHT (zip vs as) t
    _ -> Nothing
expandSyn _ _ _ = Nothing

substHT :: [(HSymbol, HType)] -> HType -> HType
substHT r (HTApp f a) = HTApp (substHT r f) (substHT r a)
substHT r t@(HTVar v) =
    case lookup v r of
    Nothing -> t
    Just t' -> t'
substHT _ t@(HTCon _) = t
substHT r (HTTuple ts) = HTTuple (map (substHT r) ts)
substHT r (HTArrow f a) = HTArrow (substHT r f) (substHT r a)
substHT r (HTUnion (ctss)) = HTUnion [ (c, map (substHT r) ts) | (c, ts) <- ctss ]


-------------------------------


data HClause = HClause HSymbol [HPat] HExpr
    deriving (Show, Eq)

data HPat = HPVar HSymbol | HPCon HSymbol | HPTuple [HPat] | HPAt HSymbol HPat | HPApply HPat HPat
    deriving (Show, Eq)

data HExpr = HELam [HPat] HExpr | HEApply HExpr HExpr | HECon HSymbol | HEVar HSymbol | HETuple [HExpr] |
	HECase HExpr [(HPat, HExpr)]
    deriving (Show, Eq)

hPrClause :: HClause -> String
hPrClause c = renderStyle style $ ppClause 0 c

ppClause :: Int -> HClause -> Doc
ppClause _p (HClause f ps e) = text f <+> sep [sep (map (ppPat 10) ps) <+> text "=",
					       nest 2 $ ppExpr 0 e]

ppPat :: Int -> HPat -> Doc
ppPat _ (HPVar s) = text s
ppPat _ (HPCon s) = text s
ppPat _ (HPTuple ps) = parens $ fsep $ punctuate comma (map (ppPat 0) ps)
ppPat _ (HPAt s p) = text s <> text "@" <> ppPat 10 p
ppPat p (HPApply a b) = pparens (p > 1) $ ppPat 1 a <+> ppPat 2 b

ppExpr :: Int -> HExpr -> Doc
ppExpr p (HELam ps e) = pparens (p > 0) $ sep [ text "\\" <+> sep (map (ppPat 10) ps) <+> text "->",
						ppExpr 0 e]
ppExpr p (HEApply f a) = pparens (p > 1) $ ppExpr 1 f <+> ppExpr 2 a
ppExpr _ (HECon s) = text s
ppExpr _ (HEVar s) = text s
ppExpr _ (HETuple es) = parens $ fsep $ punctuate comma (map (ppExpr 0) es)
ppExpr _ (HECase s alts) = (text "case" <+> ppExpr 0 s <+> text "of") $$
			    vcat (map ppAlt alts)
  where ppAlt (p, e) = ppPat 0 p <+> text "->" <+> ppExpr 0 e


pparens :: Bool -> Doc -> Doc
pparens True d = parens d
pparens False d = d

-------------------------------

unSymbol :: Symbol -> HSymbol
unSymbol (Symbol s) = s

termToHExpr :: Term -> HExpr
termToHExpr term = etaReduce $ remUnusedVars $ fst $ conv [] term
  where conv _vs (Var s) = (HEVar $ unSymbol s, [])
	conv vs (Lam s te) = 
		let hs = unSymbol s
		    (te', ss) = conv (hs : vs) te
		in  (hELam [convV hs ss] te', ss)
	conv vs (Apply (Cinj (ConsDesc s n) _) a) = (f $ foldl HEApply (HECon s) as, ss)
		where (f, as) = unTuple n ha
		      (ha, ss) = conv vs a
	conv vs (Apply te1 te2) = convAp vs te1 [te2]
	conv _vs (Ctuple 0) = (HECon "()", [])
	conv _vs e = error $ "termToHExpr " ++ show e

	unTuple 0 _ = (id, [])
	unTuple 1 a = (id, [a])
	unTuple n (HETuple as) | length as == n = (id, as)
	unTuple n e = error $ "unTuple: unimplemented " ++ show (n, e)

	unTupleP 0 _ = []
--	unTupleP 1 p = [p]
	unTupleP n (HPTuple ps) | length ps == n = ps
	unTupleP n p = error $ "unTupleP: unimplemented " ++ show (n, p)

	convAp vs (Apply te1 te2) as = convAp vs te1 (te2:as)
	convAp vs (Ctuple n) as | length as == n =
		let (es, sss) = unzip $ map (conv vs) as
		in  (hETuple es, concat sss)
	convAp vs (Ccases cds) (se : es) =
		let (alts, ass) = unzip $ zipWith cAlt es cds
		    cAlt (Lam v e) (ConsDesc c n) =
			let hv = unSymbol v
			    (he, ss) = conv (hv : vs) e
			    ps = case lookup hv ss of
				 Nothing -> replicate n (HPVar "_")
				 Just p -> unTupleP n p
			in  ((foldl HPApply (HPCon c) ps, he), ss)
		    cAlt e _ = error $ "cAlt " ++ show e
		    (e', ess) = conv vs se
		in  (hECase e' alts, ess ++ concat ass)
	convAp vs (Csplit n) (b : a : as) =
		let (hb, sb) = conv vs b
		    (a', sa) = conv vs a
		    (as', sss) = unzip $ map (conv vs) as
		    (ps, b') = unLam n hb
		    unLam 0 e = ([], e)
		    unLam k (HELam ps0 e) | length ps0 >= n = let (ps1, ps2) = splitAt k ps0 in (ps1, hELam ps2 e)
		    unLam k e = error $ "unLam: unimplemented" ++ show (k, e)
		in  case a' of
			HEVar v | v `elem` vs && null as -> (b', [(v, HPTuple ps)] ++ sb ++ sa)
			_ -> (foldr HEApply (hECase a' [(HPTuple ps, b')]) as',
			      sb ++ sa ++ concat sss)
		    
	convAp vs f as = 
		let (es, sss) = unzip $ map (conv vs) (f:as)
		in  (foldl1 HEApply es, concat sss)

	convV hs ss =
		case lookup hs ss of
		Nothing -> HPVar hs
		Just p -> HPAt hs p

	hELam [] e = e
	hELam ps (HELam ps' e) = HELam (ps ++ ps') e
	hELam ps e = HELam ps e

	hETuple [e] = e
	hETuple es = HETuple es

	hECase e [] = HEApply (HEVar "void") e
	hECase _ [(HPCon "()", e)] = e
	hECase e [(p, e')] | eqPatExpr p e' = e
	hECase e [(p, HELam ps b)] = HELam ps $ hECase e [(p, b)]
	hECase _ ((_,e):alts@(_:_)) | all (alphaEq e . snd) alts = e	-- if all arms are equal and there
								   -- at least two alternatives there can be no bound vars
	hECase e alts = HECase e alts

	eqPatExpr (HPVar s) (HEVar s') = s == s'
	eqPatExpr (HPCon s) (HECon s') = s == s'
	eqPatExpr (HPTuple ps) (HETuple es) = and (zipWith eqPatExpr ps es)
	eqPatExpr (HPApply pf pa) (HEApply ef ea) = eqPatExpr pf ef && eqPatExpr pa ea
	eqPatExpr _ _ = False

	alphaEq e1 e2 = e1 == e2	-- XXX needs improvement

termToHClause :: HSymbol -> Term -> HClause
termToHClause i term =
    case termToHExpr term of
    HELam ps e -> HClause i ps e
    e -> HClause i [] e

remUnusedVars :: HExpr -> HExpr
remUnusedVars expr = fst $ remE expr
  where remE (HELam ps e) =
	    let (e', vs) = remE e
	    in  (HELam (map (remP vs) ps) e', vs)
	remE (HEApply f a) =
	    let (f', fs) = remE f
		(a', as) = remE a
	    in  (HEApply f' a', fs ++ as)
	remE (HETuple es) =
	    let (es', sss) = unzip (map remE es)
	    in  (HETuple es', concat sss)
	remE (HECase e alts) =
	    let (e', es) = remE e
		(alts', sss) = unzip [ let (ee', ss) = remE ee in ((remP ss p, ee'), ss) | (p, ee) <- alts ]
	    in  (HECase e' alts', es ++ concat sss)
	remE e@(HECon _) = (e, [])
	remE e@(HEVar v) = (e, [v])
	remP vs p@(HPVar v) = if v `elem` vs then p else HPVar "_"
	remP _vs p@(HPCon _) = p
	remP vs (HPTuple ps) = HPTuple (map (remP vs) ps)
	remP vs (HPAt v p) = if v `elem` vs then HPAt v (remP vs p) else remP vs p
	remP vs (HPApply f a) = HPApply (remP vs f) (remP vs a)

getBinderVars :: HClause -> [HSymbol]
getBinderVars (HClause _ pats expr) = concatMap gbPat pats ++ gbExp expr
  where gbExp (HELam ps e) = concatMap gbPat ps ++ gbExp e
	gbExp (HEApply f a) = gbExp f ++ gbExp a
	gbExp (HETuple es) = concatMap gbExp es
	gbExp (HECase se alts) = gbExp se ++ concatMap (\ (p, e) -> gbPat p ++ gbExp e) alts
	gbExp _ = []
	gbPat (HPVar s) = [s]
	gbPat (HPCon _) = []
	gbPat (HPTuple ps) = concatMap gbPat ps
	gbPat (HPAt s p) = s : gbPat p
	gbPat (HPApply f a) = gbPat f ++ gbPat a

etaReduce :: HExpr -> HExpr
etaReduce expr = fst $ eta expr
  where eta (HELam [HPVar v] (HEApply f (HEVar v'))) | v == v' && v `notElem` vs = (f', vs)
	    where (f', vs) = eta f
	eta (HELam ps e) = (HELam ps e', vs) where (e', vs) = eta e
	eta (HEApply f a) = (HEApply f' a', fvs++avs) where (f', fvs) = eta f; (a', avs) = eta a
	eta e@(HECon _) = (e, [])
	eta e@(HEVar s) = (e, [s])
	eta (HETuple es) = (HETuple es', concat vss) where (es', vss) = unzip $ map eta es
	eta (HECase e alts) = (HECase e' alts', vs ++ concat vss) where (e', vs) = eta e
									(alts', vss) = unzip $ [ let (a', ss) = eta a in ((p, a'), ss)
												 | (p, a) <- alts ]
