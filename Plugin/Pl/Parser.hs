{-# LANGUAGE PatternGuards #-}

-- TODO, use Language.Haskell
-- Doesn't handle string literals?

module Plugin.Pl.Parser (parsePF) where

import Plugin.Pl.Common

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

-- is that supposed to be done that way?
tp :: T.TokenParser ()
tp = T.makeTokenParser $ haskellStyle {
  reservedNames = ["if","then","else","let","in"]
}

parens :: Parser a -> Parser a
parens = T.parens tp

brackets :: Parser a -> Parser a
brackets = T.brackets tp

symbol :: String -> Parser String
symbol = T.symbol tp

modIdentifier :: Parser String
modIdentifier = T.lexeme tp $ do
  c <- oneOf ['A'..'Z']
  cs <- many ( alphaNum <|> oneOf "_'.")
  return (c:cs)

atomic :: Parser String
atomic = try (string "()") <|> try (show `fmap` T.natural tp) <|> modIdentifier <|> T.identifier tp

reserved :: String -> Parser ()
reserved = T.reserved tp

charLiteral :: Parser Char
charLiteral = T.charLiteral tp

stringLiteral :: Parser String
stringLiteral = T.stringLiteral tp

table :: [[Operator Char st Expr]]
table = addToFirst def $ map (map inf) operators where
  addToFirst y (x:xs) = ((y:x):xs)
  addToFirst _ _ = assert False bt

  def :: Operator Char st Expr
  def = Infix (try $ do
      name <- parseOp
      guard $ not $ isJust $ lookupOp name
      spaces
      return $ \e1 e2 -> App (Var Inf name) e1 `App` e2
    ) AssocLeft

  inf :: (String, (Assoc, Int)) -> Operator Char st Expr
  inf (name, (assoc, _)) = Infix (try $ do
      string name
      notFollowedBy $ oneOf opchars
      spaces
      let name' = if head name == '`'
                  then tail . reverse . tail . reverse $ name
                  else name
      return $ \e1 e2 -> App (Var Inf name') e1 `App` e2
    ) assoc


parseOp :: CharParser st String
parseOp = (between (char '`') (char '`') $ many1 (letter <|> digit))
  <|> try (do
    op <- many1 $ oneOf opchars
    guard $ not $ op `elem` reservedOps
    return op)

pattern :: Parser Pattern
pattern = buildExpressionParser ptable ((PVar `fmap`
                       (    atomic
                        <|> (symbol "_" >> return "")))
                        <|> parens pattern)
    <?> "pattern" where
  ptable = [[Infix (symbol ":" >> return PCons) AssocRight],
            [Infix (symbol "," >> return PTuple) AssocNone]]

lambda :: Parser Expr
lambda = do
    symbol "\\"
    vs <- many1 pattern
    symbol "->"
    e <- myParser False
    return $ foldr Lambda e vs
  <?> "lambda abstraction"

var :: Parser Expr
var = try (makeVar `fmap` atomic <|>
           parens (try unaryNegation <|> try rightSection
                   <|> try (makeVar `fmap` many1 (char ','))
                   <|> tuple) <|> list <|> (Var Pref . show) `fmap` charLiteral
                   <|> stringVar `fmap` stringLiteral)
        <?> "variable" where
  makeVar v | Just _ <- lookupOp v = Var Inf v -- operators always want to
                                               -- be infixed
            | otherwise            = Var Pref v
  stringVar :: String -> Expr
  stringVar str = makeList $ (Var Pref . show) `map` str

list :: Parser Expr
list = msum (map (try . brackets) plist) <?> "list" where
  plist = [
    foldr (\e1 e2 -> cons `App` e1 `App` e2) nil `fmap`
      (myParser False `sepBy` symbol ","),
    do e <- myParser False
       symbol ".."
       return $ Var Pref "enumFrom" `App` e,
    do e <- myParser False
       symbol ","
       e' <- myParser False
       symbol ".."
       return $ Var Pref "enumFromThen" `App` e `App` e',
    do e <- myParser False
       symbol ".."
       e' <- myParser False
       return $ Var Pref "enumFromTo" `App` e `App` e',
    do e <- myParser False
       symbol ","
       e' <- myParser False
       symbol ".."
       e'' <- myParser False
       return $ Var Pref "enumFromThenTo" `App` e `App` e' `App` e''
    ]

tuple :: Parser Expr
tuple = do
    elts <- myParser False `sepBy` symbol ","
    guard $ length elts /= 1
    let name = Var Pref $ replicate (length elts - 1) ','
    return $ foldl App name elts
  <?> "tuple"

unaryNegation :: Parser Expr
unaryNegation = do
    symbol "-"
    e <- myParser False
    return $ Var Pref "negate" `App` e
  <?> "unary negation"

rightSection :: Parser Expr
rightSection = do
    v <- Var Inf `fmap` parseOp
    spaces
    let rs e = flip' `App` v `App` e
    option v (rs `fmap` myParser False)
  <?> "right section"


myParser :: Bool -> Parser Expr
myParser b = lambda <|> expr b

expr :: Bool -> Parser Expr
expr b = buildExpressionParser table (term b) <?> "expression"

decl :: Parser Decl
decl = do
  f <- atomic
  args <- pattern `endsIn` symbol "="
  e <- myParser False
  return $ Define f (foldr Lambda e args)

letbind :: Parser Expr
letbind = do
  reserved "let"
  ds <- decl `sepBy` symbol ";"
  reserved "in"
  e <- myParser False
  return $ Let ds e

ifexpr :: Parser Expr
ifexpr = do
  reserved "if"
  p <- myParser False
  reserved "then"
  e1 <- myParser False
  reserved "else"
  e2 <- myParser False
  return $ if' `App` p `App` e1 `App` e2

term :: Bool -> Parser Expr
term b = application <|> lambda <|> letbind <|> ifexpr <|>
    (guard b >> (notFollowedBy (noneOf ")") >> return (Var Pref "")))
  <?> "simple term"

application :: Parser Expr
application = do
    e:es <- many1 $ var <|> parens (myParser True)
    return $ foldl App e es
  <?> "application"

endsIn :: Parser a -> Parser b -> Parser [a]
endsIn p end = do
  xs <- many p
  end
  return $ xs

input :: Parser TopLevel
input = do
  spaces
  tl <- try (do
      f    <- atomic
      args <- pattern `endsIn` symbol "="
      e    <- myParser False
      return $ TLD True $ Define f (foldr Lambda e args)
    ) <|> TLE `fmap` myParser False
  eof
  return tl

parsePF :: String -> Either String TopLevel
parsePF inp = case runParser input () "" inp of
    Left err -> Left $ show err
    Right e  -> Right $ mapTopLevel postprocess e


postprocess :: Expr -> Expr
postprocess (Var f v) = (Var f v)
postprocess (App e1 (Var Pref "")) = postprocess e1
postprocess (App e1 e2) = App (postprocess e1) (postprocess e2)
postprocess (Lambda v e) = Lambda v (postprocess e)
postprocess (Let ds e) = Let (mapDecl postprocess `map` ds) $ postprocess e where
  mapDecl :: (Expr -> Expr) -> Decl -> Decl
  mapDecl f (Define foo e') = Define foo $ f e'

