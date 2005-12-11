module HTypes(HType, HSymbol, hTypeToFormula, pHSymbol, pHType,
	HClause, HPat, HExpr, hPrClause, termToHExpr, termToHClause) where
import Text.PrettyPrint.HughesPJ(Doc, renderStyle, style, text, (<>), parens, ($$), vcat, punctuate, sep, nest, comma, (<+>))
import Char(isAlphaNum, isLower)
import Text.ParserCombinators.ReadP
import LJTFormula

type HSymbol = String

data HType
	= HTApp HType HType
	| HTVar HSymbol
	| HTCon HSymbol
	| HTList HType
	| HTTuple [HType]
	| HTArrow HType HType
	deriving (Eq)

instance Show HType where
    showsPrec p (HTApp f a) = showParen (p > 2) $ showsPrec 2 f . showString " " . showsPrec 3 a
    showsPrec _ (HTVar s) = showString s
    showsPrec _ (HTCon s) = showString s
    showsPrec _ (HTList t) = showString "[" . showsPrec 0 t . showString "]"
    showsPrec _ (HTTuple ss) = showParen True $ f ss
	where f [] = error "showsPrec HType"
	      f [t] = showsPrec 0 t
	      f (t:ts) = showsPrec 0 t . showString ", " . f ts
    showsPrec p (HTArrow s t) = showParen (p > 0) $ showsPrec 1 s . showString " -> " . showsPrec 0 t

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

pHTAtom :: ReadP HType
pHTAtom = pHTVar +++ pHTList +++ pParen pHTTuple +++ pParen pHType +++ pUnit

pUnit :: ReadP HType
pUnit = do
    schar '('
    char ')'
    return $ HTCon "()"

pHTVar :: ReadP HType
pHTVar = do
    cs <- pHSymbol
    return $ if isLower (head cs) then HTVar cs else HTCon cs

pHSymbol :: ReadP HSymbol
pHSymbol = do
    skipSpaces
    let isSym c = isAlphaNum c || c == '\'' || c == '.'
    munch1 isSym

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
    return $ HTList t

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
hTypeToFormula ss (HTApp (HTApp (HTCon "Either") l) r) = hTypeToFormula ss l :| hTypeToFormula ss r
hTypeToFormula ss (HTTuple ts) = Conj (map (hTypeToFormula ss) ts)
hTypeToFormula ss (HTArrow t1 t2) = hTypeToFormula ss t1 :-> hTypeToFormula ss t2
hTypeToFormula _  (HTCon "()") = Truth
hTypeToFormula _  (HTCon "Void") = Falsity
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
substHT r (HTList t) = HTList (substHT r t)
substHT r (HTTuple ts) = HTTuple (map (substHT r) ts)
substHT r (HTArrow f a) = HTArrow (substHT r f) (substHT r a)


-------------------------------


data HClause = HClause HSymbol [HPat] HExpr
    deriving (Show)

data HPat = HPVar HSymbol | HPCon HSymbol | HPTuple [HPat] | HPAt HSymbol HPat | HPApply HPat HPat
    deriving (Show)

data HExpr = HELam [HPat] HExpr | HEApply HExpr HExpr | HECon HSymbol | HEVar HSymbol | HETuple [HExpr] |
	HECase HExpr [(HPat, HExpr)]
    deriving (Show)

hPrClause :: HClause -> String
hPrClause c = renderStyle style $ ppClause 0 c

ppClause :: Int -> HClause -> Doc
ppClause _p (HClause f ps e) = text f <+> sep (map (ppPat 10) ps) <+> text "=" <+> ppExpr 0 e

ppPat :: Int -> HPat -> Doc
ppPat _ (HPVar s) = text s
ppPat _ (HPCon s) = text s
ppPat _ (HPTuple ps) = parens $ sep $ punctuate comma (map (ppPat 0) ps)
ppPat _ (HPAt s p) = text s <> text "@" <> ppPat 10 p
ppPat p (HPApply a b) = pparens (p > 1) $ ppPat 1 a <+> ppPat 2 b

ppExpr :: Int -> HExpr -> Doc
ppExpr p (HELam ps e) = pparens (p > 0) $ text "\\ " <> sep (map (ppPat 10) ps) <> text " -> " <> ppExpr 0 e
ppExpr p (HEApply f a) = pparens (p > 1) $ ppExpr 1 f <+> ppExpr 2 a
ppExpr _ (HECon s) = text s
ppExpr _ (HEVar s) = text s
ppExpr _ (HETuple es) = parens $ sep $ punctuate comma (map (ppExpr 0) es)
ppExpr _ (HECase s alts) = (text "case" <+> ppExpr 0 s <+> text "of") $$
			  nest 2 (vcat (map ppAlt alts))
  where ppAlt (p, e) = ppPat 0 p <> text " -> " <> ppExpr 0 e


pparens :: Bool -> Doc -> Doc
pparens True d = parens d
pparens False d = d

-------------------------------

unSymbol :: Symbol -> HSymbol
unSymbol (Symbol s) = s

termToHExpr :: Term -> HExpr
termToHExpr term = remUnusedVars $ fst $ conv [] term
  where conv _vs (Var s) = (HEVar $ unSymbol s, [])
	conv vs (Lam s te) = 
		let hs = unSymbol s
		    (te', ss) = conv (hs : vs) te
		in  case lookup hs ss of
		    Nothing -> (hELam [HPVar hs] te', ss)
		    Just p -> (hELam [HPAt hs p] te', ss)
	conv vs (Apply te1 te2) = convAp vs te1 [te2]
	conv _vs Cleft = (HECon "Left", [])
	conv _vs Cright = (HECon "Right", [])
	conv _vs Cunit = (HECon "()", [])
	conv _vs Cabsurd = (HECon "void", [])
	conv _vs e = error $ "termToHExpr " ++ show e

	convAp vs (Apply te1 te2) as = convAp vs te1 (te2:as)
	convAp vs (Ctuple n) as | length as == n =
		let (es, sss) = unzip $ map (conv vs) as
		in  (HETuple es, concat sss)
	convAp vs Ceither [Lam lv le, Lam rv re, e] =
		let (le', ls) = conv vs le
		    (re', rs) = conv vs re
		    (e', ss) = conv vs e
		in  (HECase e' [(HPApply (HPCon "Left" ) (HPVar (unSymbol lv)), le'),
				(HPApply (HPCon "Right") (HPVar (unSymbol rv)), re')],
		     ls ++ rs ++ ss)
	convAp vs (Csplit n) (b : a : as) =
		let (HELam ps b', sb) = conv vs b
		    (a', sa) = conv vs a
		    (as', sss) = unzip $ map (conv vs) as
		    (ps1, ps2) = splitAt n ps
		in  case a' of
			HEVar v | v `elem` vs && null as && length ps == n -> (b', [(v, HPTuple ps)] ++ sb ++ sa)
			_ -> (foldr HEApply (HECase a' [(HPTuple ps1, hELam ps2 b')]) as',
			      sb ++ sa ++ concat sss)
		    
	convAp vs f as = 
		let (es, sss) = unzip $ map (conv vs) (f:as)
		in  (foldl1 HEApply es, concat sss)

	hELam [] e = e
	hELam ps (HELam ps' e) = HELam (ps ++ ps') e
	hELam ps e = HELam ps e

termToHClause :: HSymbol -> Term -> HClause
termToHClause i term =
    case termToHExpr term of
    HELam ps e -> HClause i ps e
    e -> HClause i [] e

remUnusedVars :: HExpr -> HExpr
remUnusedVars expr = fst $ remE expr
  where remE (HELam ps e) =
	    let (e', vs) = remE e
	    in  (HELam (map (remEP vs) ps) e', vs)
	remE (HEApply f a) =
	    let (f', fs) = remE f
		(a', as) = remE a
	    in  (HEApply f' a', fs ++ as)
	remE (HETuple es) =
	    let (es', sss) = unzip (map remE es)
	    in  (HETuple es', concat sss)
	remE (HECase e alts) =
	    let (e', es) = remE e
		(alts', sss) = unzip [ let (ee', ss) = remE ee in ((p, ee'), ss) | (p, ee) <- alts ]
	    in  (HECase e' alts', es ++ concat sss)
	remE e@(HECon _) = (e, [])
	remE e@(HEVar v) = (e, [v])
	remEP vs p@(HPVar v) = if v `elem` vs then p else HPVar "_"
	remEP _vs p@(HPCon _) = p
	remEP vs (HPTuple ps) = HPTuple (map (remEP vs) ps)
	remEP vs (HPAt v p) = if v `elem` vs then HPAt v (remEP vs p) else remEP vs p
	remEP vs (HPApply f a) = HPApply (remEP vs f) (remEP vs a)