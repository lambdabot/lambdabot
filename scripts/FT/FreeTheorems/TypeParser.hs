-- Copyright 2006, Sascha Boehme.




-- | This module defines a parser to create a 'NamedType' out of a string.
--   The parser itself is based on Parsec.

module FreeTheorems.TypeParser (
    parseNamedType,
    parseType,

    -- testing interface
--  testTypeParser
) where



import FreeTheorems.Declarations
import FreeTheorems.Preparation
import FreeTheorems.PrettyPrint
import FreeTheorems.Types
import Data.Char (isLower, isUpper, isAlphaNum, isSpace, isDigit)
import Text.ParserCombinators.Parsec

-- import FreeTheorems.Test.ArbitraryTypes
-- import Test.QuickCheck (quickCheck)





-- | Parses a Haskell type string and creates a named type.
--
--   The content of the string argument has to be conforming to the following
--   EBNF syntax:
--
-- > NamedType ::= Name :: TypeTerm
--
--   where @Name@ is a token starting with a lower letter or @_@, and
--   consisting of alphanumeric letters and the characters @_@ and @'@ from the
--   second character onwards. A @Name@ starting with @_@ has to be at least two
--   characters long, because @_@ is a reserved identifier in Haskell.
--
--   Alternatively, a @Name@ can also be an operator which has to be enclosed in
--   parentheses. Allowed characters of operators are
--
-- > ! | # | $ | % | & | * | + | . | / | < | = | > | ? | @ | \ | ^ | | | - | ~
--
--   and the colon @:@ from the second although character onwards.
--   Note that the following operators are restricted keywords in Haskell and
--   can not be used as names of types:
--
-- > .. | : | :: | = | \ | | | <- | -> | @ | ~ | =>
--
--   For the syntax of a @TypeTerm@ see 'parseType'.

parseNamedType :: String                            -- ^ The string to be
                                                    --   parsed.
                  -> Either ParseError NamedType    -- ^ Returns the parsed
                                                    --   named type or a parser
                                                    --   error.
parseNamedType string =
  case runParser namedType () "" string of
    Left error             -> Left error
    Right (NamedType tv t) -> Right (NamedType tv (prepare t))



-- | Parses a Haskell type string and creates a named type.
--
--   A type string has to be conforming to the following EBNF syntax:
--
-- > TypeTerm ::= SimpleTypeTerm
-- >            | SimpleTypeTerm "->" TypeTerm
-- >            | "forall" TypeVariableList "." TypeTerm
--
-- > SimpleTypeTerm ::= ATypeTerm
-- >                  | TypeConstructor ATypeTermList
-- >                  | "[]" ATypeTerm
-- >                  | "(,)" ATypeTerm ATypeTerm
-- >                  | "(,,)" ATypeTerm ATypeTerm ATypeTerm
-- >                  | ...
-- >                  | {- tuple in prefix notation of arity 15 -}
-- >                  | "(->)" ATypeTerm ATypeTerm
--
-- > ATypeTerm ::= "Char" | "Int" | "Integer" | "Float" | "Double"
-- >             | TypeVariable
-- >             | "()"
-- >             | "[" TypeTerm "]"
-- >             | "(" TypeTerm "," TypeTerm ")"
-- >             | "(" TypeTerm "," TypeTerm "," TypeTerm ")"
-- >             | ...
-- >             | {- tuple of arity 15 -}
-- >             | "(" TypeTerm ")"
--
-- > ATypeTermList ::= ATypeTerm
-- >                 | ATypeTermList ATypeTerm
--
-- > TypeVariableList ::= TypeVariable
-- >                    | TypeVariableList TypeVariable
--
--   where @TypeVariable@ is a token starting with a lower letter or @_@, while
--   @TypeConstructor@ starts with a capital letter. Every of these two tokens
--   may consist of alphanumeric letters and the characters @_@ and @'@ from the
--   second character onwards.
--   A @TypeVariable@ starting with @_@ has to be at least two characters long,
--   because @_@ is a reserved identifier in Haskell.
--   Type constructors can not be formed from symbols.
--
--   Although tuples are restricted to an arity of 15, tuples in the usual
--   non-prefixed way can be arbitrary large. However, applications should not
--   rely on this possibility.
--
--   An additional rule limits the valid type terms: The number of type
--   arguments for a type constructor must match the type constructors arity.

parseType :: (String -> Bool)       -- ^ When 'parseType' is trying to
                                    --   automatically create a name for the
                                    --   type, this function is asked if a name
                                    --   is acceptable.

             -> String              -- ^ The string to be parsed.

             -> Either ParseError NamedType
                                    -- ^ Returns the parsed named type with
                                    --   the generated name or an parser
                                    --   error.
parseType accept string =
  case runParser onlyType () "" string of
    Left error -> Left error
    Right t    -> Right (NamedType (newName (TV "t" 1) accept) (prepare t))
  where
    newName v@(TV t i) accept = if accept (printAsText v)
                                  then v
                                  else newName (TV t (i+1)) accept



--------------------------------------------------------------------------------



-- Parser for the above syntax.



-- | The parser type used throughout this module.
--   This type was just defined to make the type following annotations shorter.

type CLParser a = GenParser Char () a



-- | Parses a named type.
--   This is a top-level parser, i.e. it requests end of input when finished.

namedType :: CLParser NamedType
namedType = do
  skipSpace
  n <- (typeName <?> "name of a type")
  symbol "::"
  t <- typeTerm
  eof

  -- separate the parsed name in name and index, if possible
  let (rd, rn) = span isDigit $ reverse n
  let (n', digits) = (reverse rn, reverse rd)
  let tv = if (digits == [])
             then PV n'
             else TV n' (read digits)

  return (NamedType tv t)



-- | Parses a type only.
--   This is a top-level parser, i.e. it requests end of input when finished.

onlyType :: CLParser Type
onlyType = do
  skipSpace
  t <- typeTerm
  eof
  return t



-- | Parses a type term consisting of functions or type abstractions,
--   both of which are based on simple type terms.

typeTerm :: CLParser Type
typeTerm =

  -- parse type abstractions
  -- This is different to the above EBNF definition to allow for finding
  -- the keyword "forall" before any type variables. Thus, the parser gets
  -- a bit simpler.
      do try (keyword "forall")
         vs <- many1 typeVariable
         symbol "."
         t <- typeTerm
         return (foldr (\v t -> TypeForall v t) t vs)

  -- parse a simple type term possibly being part of a function type
  <|> simpleTypeTerm `chainr1` function



-- | Parses the function symbol and returns a function to create a function
--   type term out of two type terms.

function :: CLParser (Type -> Type -> Type)
function = do
  symbol "->"
  return (\t1 t2 -> TypeFun t1 t2)



-- | Parses a simple type term.

--   The cases are ordered so that first predefined type constructors in prefix
--   notatation are checked, then basic type terms, and finally type
--   constructors are parsed.

simpleTypeTerm :: CLParser Type
simpleTypeTerm =
      do try (symbol "[]" <?> "prefixed list constructor")
         t <- aTypeTerm
         return (TypeList t)

  <|> do n <- (tupleConstructor <?> "prefixed tuple constructor")
         ts <- count n aTypeTerm
         return (TypeTuple ts)

  <|> do try (symbol "(->)" <?> "prefixed function constructor")
         t1 <- aTypeTerm
         t2 <- aTypeTerm
         return (TypeFun t1 t2)

  <|> aTypeTerm

  <|> do (c, n) <- (typeConstructor <?> "type constructor")
         ts <- count n aTypeTerm
         return (TypeCon c ts)

  <?> "a simple type term"



-- | Parses a basic type term.

aTypeTerm :: CLParser Type
aTypeTerm =
      do try (keyword "Char")
         return (TypeBasic Char)
  <|> do try (keyword "Int")
         return (TypeBasic Int)
  <|> do try (keyword "Integer")
         return (TypeBasic Integer)
  <|> do try (keyword "Float")
         return (TypeBasic Float)
  <|> do try (keyword "Double")
         return (TypeBasic Double)

  <|> do v <- (typeVariable <?> "type variable")
         return (TypeVar v)

  <|> do t <- between (symbol "[") (symbol "]") typeTerm
         return (TypeList t)

  <|> do l <- between (symbol "(") (symbol ")") (typeTerm `sepBy` (symbol ","))
         return $ case l of
                    []        -> TypeUnit
                    [t]       -> t
                    otherwise -> (TypeTuple l)

  <?> ("a basic type, a type variable or a list or tuple type term "
       ++ "like [a] or (a,b)")


--------------------------------------------------------------------------------



-- The lexer for the above parser's tokens.
-- After every token, spaces are skipped.


-- | Parses a name (of an identifier).

name :: CLParser String
name =
      do c  <- char '_'
         cs <- (many1 hsID <?> "at least one more character after \"_\"")
         skipSpace
         return (c:cs)

  <|> do c  <- hsSmall
         cs <- many hsID
         skipSpace
         return (c:cs)



-- | Parses a name of a type which can be either an identifier or an operator.

typeName :: CLParser String
typeName =
     name
 <|> do char '('
        op1 <- hsOperator1
        ops <- many hsOperator
        let op = op1 : ops
        if any (== op) reservedOps
          then unexpected ("reserved operator \"" ++ op ++ "\"")
          else do char ')'
                  skipSpace
                  return ("(" ++ op ++ ")")
  where
    reservedOps =
      [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]



-- | Parses a type variable. Note that there can not be a type variable called
--   "forall".

typeVariable :: CLParser TypeVariable
typeVariable = do
  n <- name
  if (n == "forall")
    then unexpected "reserved word \"forall\"" <?> "type variable"
    else return n



-- | Parses a type constructor.
--   This function checks, if the parsed type constructor is accepted in the
--   given language subset. If so, it returns also the arity of the type
--   constructor.
--   If the parsed type constructor is not valid, an error message is created.

typeConstructor :: CLParser (TypeConstructor, Int)
typeConstructor = do
  c  <- hsLarge
  cs <- many hsID
  let con = c:cs
  if (not $ isDefined con)
    then unexpected ("type constructor '" ++ con ++ "' " ++
                     "(this type constructor is not declared)")
    else do
      let Just n = getArity con
      skipSpace
      return (con, n)



-- | Parses a tuple type constructor as @(,)@ or @(,,)@.
--   Returns the arity of the parsed tuple type constructor.

tupleConstructor :: CLParser Int
tupleConstructor = do
  try (string "(,")
  cs <- many (char ',')
  char ')'
  skipSpace
  return (length cs + 2)



-- | Checks if a symbol (consisting of operator characters) is at the current
--   position of the input.

symbol :: String -> CLParser ()
symbol s = do
  string s
  skipSpace
  return ()



-- | Checks if a keyword (consisting of alphanumeric characters) is at the
--   current position of the input. Assures also, that after this keyword no
--   other alphanumeric character is following.

keyword :: String -> CLParser ()
keyword s = do
  string s
  notFollowedBy hsID
  skipSpace
  return ()



-- | Skips zero or more space characters.

skipSpace :: CLParser ()
skipSpace = skipMany hsSpace



-- | Returns a lower letter character (see 'name').

hsSmall :: CLParser Char
hsSmall = satisfy $ \c -> isLower c



-- | Returns a capital letter character (see 'typeConstructor').

hsLarge :: CLParser Char
hsLarge = satisfy $ \c -> isUpper c



-- | Returns an alphanumeric character of which keywords and identifiers can be
--   formed.

hsID :: CLParser Char
hsID = satisfy $ \c -> isAlphaNum c || (c == '_') || (c == '\'')



-- | Returns a symbol which can be the first character of an operator function.

hsOperator1 :: CLParser Char
hsOperator1 = satisfy $ \c -> any (== c) "!#$%&*+./<=>?@\\^|-~"



-- | Returns a symbol of which operators can be formed.

hsOperator :: CLParser Char
hsOperator = satisfy $ \c -> any (== c) "!#$%&*+./<=>?@\\^|-~:"



-- | Returns a space character.

hsSpace :: CLParser Char
hsSpace = satisfy $ \c -> isSpace c



--------------------------------------------------------------------------------



-- Helper function



-- | Returns the arity of a type constructor or @Nothing@ if the type
--   constructor is not accepted in the given language subset.

getArity :: TypeConstructor -> Maybe Int
getArity con =
  case (getDataDecl con) of
    Just (DataDecl _ len _) -> Just len
    Nothing                 ->
      case (getTypeDecl con) of
        Just (TypeDecl _ len _) -> Just len
        Nothing                 -> Nothing



--------------------------------------------------------------------------------



-- A list of tests for this module.

{-
testTypeParser = do
  putStr "parser for named types works ... "
  quickCheck prop_parser_NamedType
  putStr "parser for types works ... "
  quickCheck prop_parser_Type
-}



-- Checks the type parser for named types.

prop_parser_NamedType nt =
  case parseNamedType (printAsText nt) of
    Left _    -> False
    Right nt' -> let NamedType tv t = nt
                 in  NamedType tv (prepare t) == nt'



-- Checks the type parser for types only.

prop_parser_Type t =
  let nt   = NamedType (PV "t") t
      text = drop 4 (printAsText nt)
  in  case parseType (\_ -> True) text of
        Left _                 -> False
        Right (NamedType _ t') -> prepare t == t'

