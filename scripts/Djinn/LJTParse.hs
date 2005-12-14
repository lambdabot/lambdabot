module LJTParse(parseFormula, parseLJT) where
import Char(isAlphaNum)
import Text.ParserCombinators.ReadP(ReadP, (+++), char, sepBy1, readP_to_S, skipSpaces, munch1, many)
import LJTFormula

parseFormula :: String -> Formula
parseFormula = parser pTop

parseLJT :: String -> Formula
parseLJT = parser pLJT

parser :: (Show a) => ReadP a -> String -> a
parser p s =
    let ess = readP_to_S p (removeComments s)
    in  case filter (null . snd) ess of
        [(e, "")] -> e
        _ -> error ("bad parse: " ++ show ess)

removeComments :: String -> String
removeComments "" = ""
removeComments ('%':cs) = skip cs
  where skip "" = ""
	skip s@('\n':_) = removeComments s
	skip (_:s) = skip s
removeComments (c:cs) = c : removeComments cs

pTop :: ReadP Formula
pTop = do
   f <- pFormula
   skipSpaces
   return f

pLJT :: ReadP Formula
pLJT = do
   schar 'f'
   f <- pFormula
   schar '.'
   skipSpaces
   return f

pFormula :: ReadP Formula
pFormula = do
   f1 <- pDisjuction
   ods <- many (do o <- pArrow; d <- pDisjuction; return (o, d))
   let (op, f2) = foldr (\ (no, d) (oo, r) -> (no, d `oo` r)) (const, undefined) ods
   return $ f1 `op` f2

pArrow :: ReadP (Formula -> Formula -> Formula)
pArrow =
   (do schar '-'; char '>'; return (:->))
   +++
   (do schar '<'; char '-'; char '>'; return (<->))

pDisjuction :: ReadP Formula
pDisjuction = do
   fs <- sepBy1 pConjunction (schar 'v')
   return $ foldl1 (|:) fs

pConjunction :: ReadP Formula
pConjunction = do
   fs <- sepBy1 pAtomic (schar '&')
   return $ foldl1 (&) fs

pAtomic :: ReadP Formula
pAtomic = pNegation +++ pParen pFormula +++ pVar

pNegation :: ReadP Formula
pNegation = do
    schar '~'
    f <- pAtomic
    return $ fnot f

pVar :: ReadP Formula
pVar = do
    skipSpaces
    cs <- munch1 isAlphaNum
    case cs of
	"false" -> return false
	"true" -> return true
	_ -> return $ PVar $ Symbol cs

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

