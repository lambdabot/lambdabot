{-# LANGUAGE PatternGuards #-}
module Plugin.Pl.PrettyPrinter (Expr) where

-- Dummy export to make ghc -Wall happy

import Lambdabot.Serial (readM)
import Plugin.Pl.Common

instance Show Decl where
  show (Define f e) = f ++ " = " ++ show e
  showList ds = (++) $ concat $ intersperse "; " $ map show ds

instance Show TopLevel where
  showsPrec p (TLE e) = showsPrec p e
  showsPrec p (TLD _ d) = showsPrec p d

-- | Expression with syntactic sugar
data SExpr
  = SVar !String
  | SLambda ![Pattern] !SExpr
  | SLet ![Decl] !SExpr
  | SApp !SExpr !SExpr
  | SInfix !String !SExpr !SExpr
  | LeftSection !String !SExpr  -- (x +)
  | RightSection !String !SExpr -- (+ x)
  | List ![SExpr]
  | Tuple ![SExpr]
  | Enum !Expr !(Maybe Expr) !(Maybe Expr)

{-# INLINE toSExprHead #-}
toSExprHead :: String -> [Expr] -> Maybe SExpr
toSExprHead hd tl
  | all (==',') hd, length hd+1 == length tl
  = Just . Tuple . reverse $ map toSExpr tl
  | otherwise = case (hd,reverse tl) of
      ("enumFrom", [e])              -> Just $ Enum e Nothing   Nothing
      ("enumFromThen", [e,e'])       -> Just $ Enum e (Just e') Nothing
      ("enumFromTo", [e,e'])         -> Just $ Enum e Nothing   (Just e')
      ("enumFromThenTo", [e,e',e'']) -> Just $ Enum e (Just e') (Just e'')
      _                              -> Nothing

toSExpr :: Expr -> SExpr
toSExpr (Var _ v) = SVar v
toSExpr (Lambda v e) = case toSExpr e of
  (SLambda vs e') -> SLambda (v:vs) e'
  e'              -> SLambda [v] e'
toSExpr (Let ds e) = SLet ds $ toSExpr e
toSExpr e | Just (hd,tl) <- getHead e, Just se <- toSExprHead hd tl = se
toSExpr e | (ls, tl) <- getList e, tl == nil
  = List $ map toSExpr ls
toSExpr (App e1 e2) = case e1 of
  App (Var Inf v) e0
    -> SInfix v (toSExpr e0) (toSExpr e2)
  Var Inf v | v /= "-"
    -> LeftSection v (toSExpr e2)

  Var _ "flip" | Var Inf v <- e2, v == "-" -> toSExpr $ Var Pref "subtract"

  App (Var _ "flip") (Var pr v)
    | v == "-"  -> toSExpr $ Var Pref "subtract" `App` e2
    | v == "id" -> RightSection "$" (toSExpr e2)
    | Inf <- pr -> RightSection v (toSExpr e2)
  _ -> SApp (toSExpr e1) (toSExpr e2)

getHead :: Expr -> Maybe (String, [Expr])
getHead (Var _ v) = Just (v, [])
getHead (App e1 e2) = second (e2:) `fmap` getHead e1
getHead _ = Nothing

instance Show Expr where
  showsPrec p = showsPrec p . toSExpr

instance Show SExpr where
  showsPrec _ (SVar v) = (getPrefName v ++)
  showsPrec p (SLambda vs e) = showParen (p > minPrec) $ ('\\':) .
    foldr (.) id (intersperse (' ':) (map (showsPrec $ maxPrec+1) vs)) .
    (" -> "++) . showsPrec minPrec e
  showsPrec p (SApp e1 e2) = showParen (p > maxPrec) $
    showsPrec maxPrec e1 . (' ':) . showsPrec (maxPrec+1) e2
  showsPrec _ (LeftSection fx e) = showParen True $
    showsPrec (snd (lookupFix fx) + 1) e . (' ':) . (getInfName fx++)
  showsPrec _ (RightSection fx e) = showParen True $
    (getInfName fx++) . (' ':) . showsPrec (snd (lookupFix fx) + 1) e
  showsPrec _ (Tuple es) = showParen True $
    (concat `id` intersperse ", " (map show es) ++)

  showsPrec _ (List es)
    | Just cs <- mapM ((=<<) readM . fromSVar) es = shows (cs::String)
    | otherwise = ('[':) .
      (concat `id` intersperse ", " (map show es) ++) . (']':)
    where fromSVar (SVar str) = Just str
          fromSVar _          = Nothing
  showsPrec _ (Enum fr tn to) = ('[':) . shows fr .
    showsMaybe (((',':) . show) `fmap` tn) . (".."++) .
    showsMaybe (show `fmap` to) . (']':)
      where showsMaybe = maybe id (++)
  showsPrec _ (SLet ds e) = ("let "++) . shows ds . (" in "++) . shows e


  showsPrec p (SInfix fx e1 e2) = showParen (p > fixity) $
    showsPrec f1 e1 . (' ':) . (getInfName fx++) . (' ':) .
    showsPrec f2 e2 where
      fixity = snd $ lookupFix fx
      (f1, f2) = case fst $ lookupFix fx of
        AssocRight -> (fixity+1, fixity + infixSafe e2 AssocLeft fixity)
        AssocLeft  -> (fixity + infixSafe e1 AssocRight fixity, fixity+1)
        AssocNone  -> (fixity+1, fixity+1)

      -- This is a little bit awkward, but at least seems to produce no false
      -- results anymore
      infixSafe :: SExpr -> Assoc -> Int -> Int
      infixSafe (SInfix fx'' _ _) assoc fx'
        | lookupFix fx'' == (assoc, fx') = 1
        | otherwise = 0
      infixSafe _ _ _ = 0 -- doesn't matter

instance Show Pattern where
  showsPrec _ (PVar v) = (v++)
  showsPrec _ (PTuple p1 p2) = showParen True $
    showsPrec 0 p1 . (", "++) . showsPrec 0 p2
  showsPrec p (PCons p1 p2) = showParen (p>5) $
    showsPrec 6 p1 . (':':) . showsPrec 5 p2

isOperator :: String -> Bool
isOperator = all (`elem` opchars)

getInfName :: String -> String
getInfName str = if isOperator str then str else "`"++str++"`"

getPrefName :: String -> String
getPrefName str = if isOperator str || ',' `elem` str then "("++str++")" else str

instance Eq Assoc where
  AssocLeft  == AssocLeft  = True
  AssocRight == AssocRight = True
  AssocNone  == AssocNone  = True
  _          == _          = False

{-
instance Show Assoc where
  show AssocLeft  = "AssocLeft"
  show AssocRight = "AssocRight"
  show AssocNone  = "AssocNone"

instance Ord Assoc where
  AssocNone <= _ = True
  _ <= AssocNone = False
  AssocLeft <= _ = True
  _ <= AssocLeft = False
  _ <= _ = True
-}
