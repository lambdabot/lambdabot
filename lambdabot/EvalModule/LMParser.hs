{-# OPTIONS -fglasgow-exts #-}
-- multi-parameter type classes

--
-- screw modularity (at least temporarily)
--
module EvalModule.LMParser (parseTerm,Term(..)) where

import EvalModule.LangPack
import EvalModule.ArithTerm (ArithTerm(..))
import EvalModule.LambdaTerm (LambdaTerm(..))
import EvalModule.RelTerm (RelTerm(..))
import EvalModule.ListTerm (ListTerm(..))

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (emptyDef, reservedNames)
import qualified Text.ParserCombinators.Parsec.Token as P hiding (reservedNames)

tp :: P.TokenParser st
tp = P.makeTokenParser 
        (emptyDef { reservedNames = 
                ["if","then","else","True","False","head","tail","null"] })

-- integer :: CharParser st Integer
-- integer         = P.integer tp

natural :: CharParser st Integer
natural         = P.natural tp

boolean :: GenParser Char st Bool
boolean         = (reserved "True" >> return True) 
              <|> (reserved "False" >> return False)

reserved :: String -> CharParser st ()
reserved        = P.reserved tp
 
symbol :: String -> CharParser st String
symbol          = P.symbol tp

parens, brackets :: CharParser st a -> CharParser st a
parens          = P.parens tp
brackets        = P.brackets tp

identifier, dot, comma :: CharParser st String
identifier      = P.identifier tp
dot             = P.dot tp
comma           = P.comma tp

charLiteral :: CharParser st Char
charLiteral     = P.charLiteral tp

stringLiteral :: CharParser st String
stringLiteral   = P.stringLiteral tp

identStart, identLetter :: CharParser st Char
identStart      = P.identStart emptyDef
identLetter     = P.identLetter emptyDef

class Up f x where
    up :: f x -> x

data Term x 
  = ArithT (ArithTerm x) 
  | LambdaT (LambdaTerm x) 
  | RelT (RelTerm x)
  | ListT (ListTerm x)

type Term' = Fix Term

instance Functor Term where
    fmap f (ArithT x) = ArithT (fmap f x)
    fmap f (LambdaT x) = LambdaT (fmap f x)
    fmap f (RelT x) = RelT (fmap f x)
    fmap f (ListT x) = ListT (fmap f x)

instance Up ArithTerm Term' where
    up = In . ArithT

instance Up LambdaTerm Term' where
    up = In . LambdaT

instance Up RelTerm Term' where
    up = In . RelT

instance Up ListTerm Term' where
    up = In . ListT

-- up' :: (Up f x) => Parser (f x) -> Parser x
-- up' p = p >>= return . up

up2 :: (Up f x) => (t -> t1 -> f x) -> t -> t1 -> x
up2 f = (\x y -> up $ f x y)

parseTerm :: [Char] -> Either ParseError Term'
parseTerm = parse (do spaces; r <- parser';eof; return r) ""

application :: GenParser Char () Term'
application = do v <- (var <|> listOp)
                 es <- many (var <|> listOp <|> literal <|> parens parser')
                 case es of
                    [] -> return v
                    es'-> return $ foldl1 (up2 App) (v:es')

parser' :: GenParser Char () Term'
parser' = do e <- expr
             es <- many expr'
             case es of
                [] -> return e
                es'-> return $ foldl1 (up2 App) (e:es')

var :: GenParser Char st Term'
var = (identifier >>= return . up . Var) <?> "var"

-- var without stripping trailing spaces
var' :: GenParser Char st Term'
var' = (do c <- identStart; cs <- many identLetter; return $ up $ Var (c:cs)) <?> "var"

num :: GenParser Char st Term'
num = (natural >>= return . up . Num) <?> "num"

bool :: GenParser Char st Term'
bool = (boolean >>= return . up . Boolean) <?> "bool"

character :: GenParser Char st Term'
character = (charLiteral >>= return . up . Character) <?> "character"

stringL :: GenParser Char st Term'
stringL = (stringLiteral >>= return . foldr (up2 Cons . up . Character) (up Nil)) <?> "string"

listOp :: GenParser Char st Term'
listOp = do o <- (reserved "head" >> return Head) <|>
                 (reserved "tail" >> return Tail) <|>
                 (reserved "null" >> return Null)
            return $ up2 Lam "x" (up $ o $ up $ Var "x")

list :: GenParser Char () Term'
list = do es <- brackets (parser' `sepBy` comma)
          return (foldr (up2 Cons) (up Nil) es) <?> "list"

literal :: GenParser Char () Term'
literal = bool <|> num <|> character <|> list <|> stringL

term :: GenParser Char () Term' -> GenParser Char () Term'
term x = x <|> literal <|> ifEx <|> listOp <|>
        abstraction <|> parens parser' <?> "simple term"

expr :: GenParser Char () Term'
expr = buildExpressionParser table (term application)

expr' :: GenParser Char () Term'
expr' = buildExpressionParser table (term var)

table :: OperatorTable Char () Term'
table = [[Infix (op "." (\f g -> Lam "#x#" $ up2 App f $ up2 App g (up $ Var "#x#"))) AssocRight,
          Infix (do v <- between (char '`') (symbol "`") var'; return $ up2 App . up2 App v) AssocLeft],
         [Infix (op "*" Mul) AssocLeft, Infix (try $ do char '/'; notFollowedBy (char '='); spaces; return $ up2 Div) AssocLeft],
         [Infix (op "-" Sub) AssocLeft, Infix (try $ do char '+'; notFollowedBy (char '+'); spaces; return $ up2 Add) AssocLeft],
         [Infix (op ":" Cons) AssocRight, Infix (op "++" Append) AssocRight],
         [Infix (op "<=" LessThanOrEqual) AssocNone, Infix (op ">=" GreaterThanOrEqual) AssocNone,
          Infix (op "<" LessThan) AssocNone, Infix (op ">" GreaterThan) AssocNone,
          Infix (op "==" Equal) AssocNone, Infix (op "/=" NotEqual) AssocNone],
         [Infix (op "&&" And) AssocRight], 
         [Infix (op "||" Or) AssocRight],
         [Infix (op "$" App) AssocRight]]

op :: (Up f trm) => String -> (trm -> trm -> f trm) -> Parser (trm -> trm -> trm)
op s o = try $ do symbol s; return (up2 o)

abstraction :: GenParser Char () Term'
abstraction = do symbol "\\"
                 vs <- many1 identifier
                 (dot <|> symbol "->") -- yay!
                 b <- parser'
                 return $ foldr (up2 Lam) b vs
              <?> "abstraction"

ifEx :: GenParser Char () Term'
ifEx = do reserved "if"
          c <- parser'
          reserved "then"
          t <- parser'
          reserved "else"
          e <- parser'
          return $ up $ IfE c t e
       <?> "if"
