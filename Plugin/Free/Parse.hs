{-# OPTIONS -w #-}

module Plugin.Free.Parse where

import Control.Monad

data Token
    = QVarId String
    | QConId String
    | QVarSym String
    | QConSym String
    | OpenParen
    | CloseParen
    | Comma
    | Semicolon
    | OpenBracket
    | CloseBracket
    | BackQuote
    | OpenBrace
    | CloseBrace
    | OpDotDot
    | OpColon
    | OpColonColon
    | OpEquals
    | OpBackslash
    | OpPipe
    | OpBackArrow
    | OpArrow
    | OpAt
    | OpTilde
    | OpImplies
    | IdCase
    | IdClass
    | IdData
    | IdDefault
    | IdDeriving
    | IdDo
    | IdElse
    | IdForall
    | IdIf
    | IdImport
    | IdIn
    | IdInfix
    | IdInfixl
    | IdInfixr
    | IdInstance
    | IdLet
    | IdModule
    | IdNewtype
    | IdOf
    | IdThen
    | IdType
    | IdWhere
    | IdUscore
    | TokError String
        deriving (Show,Eq,Ord)

data ParseResult a
    = ParseSuccess a [Token]
    | ParseError String
        deriving (Show)

newtype ParseS a = ParseS { parse :: [Token] -> ParseResult a }

instance Monad ParseS where
    return x = ParseS (\ts -> ParseSuccess x ts)
    m >>= k = ParseS (\ts -> case parse m ts of
                                ParseSuccess x ts' -> parse (k x) ts'
                                ParseError s       -> ParseError s)
    fail str = ParseS (\_ -> ParseError str)

instance MonadPlus ParseS where
    mzero = ParseS (\ts -> ParseError "parse error")
    mplus m1 m2
        = ParseS (\ts -> case parse m1 ts of
                            res@(ParseSuccess _ _) -> res
                            ParseError _           -> parse m2 ts)

peekToken :: ParseS (Maybe Token)
peekToken = ParseS (\ts -> case ts of
                            []     -> ParseSuccess Nothing []
                            (t':_) -> ParseSuccess (Just t') ts)

getToken :: ParseS (Maybe Token)
getToken = ParseS (\ts -> case ts of
                            []     -> ParseSuccess Nothing []
                            (t:ts) -> ParseSuccess (Just t) ts)

match :: Token -> ParseS ()
match m
    = do
        mt <- getToken
        case mt of
            Just t | t == m -> return ()
            _               -> fail ("Expected " ++ show m)

ascSymbol = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\',
                '^','|','-','~']


lexer :: String -> [Token]
lexer []
    = []
lexer (' ':cs)
    = lexer cs
lexer ('\t':cs)
    = lexer cs
lexer ('\f':cs)
    = lexer cs
lexer ('\r':cs)
    = lexer cs
lexer ('\n':cs)
    = lexer cs
lexer ('\v':cs)
    = lexer cs
lexer ('-':'-':cs)
    = lexerLineComment cs
    where
        lexerLineComment ('\r':'\n':cs) = lexer cs
        lexerLineComment ('\r':cs) = lexer cs
        lexerLineComment ('\n':cs) = lexer cs
        lexerLineComment ('\f':cs) = lexer cs
        lexerLineComment (c:cs) = lexerLineComment cs
        lexerLineComment [] = []
lexer ('{':'-':cs)
    = lexerComment lexer cs
    where
        lexerComment k ('{':'-':cs) = lexerComment (lexerComment k) cs
        lexerComment k ('-':'}':cs) = k cs
        lexerComment k (_:cs) = lexerComment k cs
        lexerComment k [] = [TokError "Unterminated comment"]
lexer ('(':cs)
    = OpenParen : lexer cs
lexer (')':cs)
    = CloseParen : lexer cs
lexer (',':cs)
    = Comma : lexer cs
lexer ('[':cs)
    = OpenBracket : lexer cs
lexer (']':cs)
    = CloseBracket : lexer cs
lexer (c@':':cs)
    = lexerConSym [c] cs
    where
        lexerConSym con (c:cs)                
            | c == ':'
                || c `elem` ascSymbol
                = lexerConSym (c:con) cs
        lexerConSym con cs
            = case reverse con of
                ":"  -> OpColon : lexer cs
                "::" -> OpColonColon : lexer cs
                con  -> QConSym con : lexer cs
lexer (c:cs)
    | c `elem` ['A'..'Z']
        = lexerConId [c] cs
    | c `elem` ['a'..'z'] || c == '_'
        = lexerVarId [c] cs
    | c `elem` ascSymbol
        = lexerVarSym [c] cs
    | otherwise
        = [TokError "Illegal char"]
        where
            lexerConId con (c:cs)
                | c `elem` ['A'..'Z']
                    || c `elem` ['a'..'z']
                    || c `elem` ['0'..'9']
                    || c == '\''
                    || c == '_'
                        = lexerConId (c:con) cs
            lexerConId con cs
                = QConId (reverse con) : lexer cs

            lexerVarId var (c:cs)
                | c `elem` ['A'..'Z']
                    || c `elem` ['a'..'z']
                    || c `elem` ['0'..'9']
                    || c == '\''
                    || c == '_'
                        = lexerVarId (c:var) cs
            lexerVarId var cs
                = case reverse var of
                    "_"        -> IdUscore : lexer cs
                    "case"     -> IdCase : lexer cs
                    "class"    -> IdClass : lexer cs
                    "data"     -> IdData : lexer cs
                    "default"  -> IdDefault : lexer cs
                    "deriving" -> IdDeriving : lexer cs
                    "do"       -> IdDo : lexer cs
                    "else"     -> IdElse : lexer cs
                    "forall"   -> IdForall : lexer cs
                    "if"       -> IdIf : lexer cs
                    "import"   -> IdImport : lexer cs
                    "in"       -> IdIn : lexer cs
                    "infix"    -> IdInfix : lexer cs
                    "infixl"   -> IdInfixl : lexer cs
                    "infixr"   -> IdInfixr : lexer cs
                    "instance" -> IdInstance : lexer cs
                    "let"      -> IdLet : lexer cs
                    "module"   -> IdModule : lexer cs
                    "newtype"  -> IdNewtype : lexer cs
                    "of"       -> IdOf : lexer cs
                    "then"     -> IdThen : lexer cs
                    "type"     -> IdType : lexer cs
                    "where"    -> IdWhere : lexer cs
                    v          -> QVarId v : lexer cs

            lexerVarSym var (c:cs)
                | c == ':' || c `elem` ascSymbol
                    = lexerVarSym (c:var) cs
            lexerVarSym var cs
                = case reverse var of
                    ".."    -> OpDotDot : lexer cs
                    "="     -> OpEquals : lexer cs
                    "\\"    -> OpBackslash : lexer cs
                    "|"     -> OpPipe : lexer cs
                    "<-"    -> OpBackArrow : lexer cs
                    "->"    -> OpArrow : lexer cs
                    "@"     -> OpAt : lexer cs
                    "~"     -> OpTilde : lexer cs
                    "=>"    -> OpImplies : lexer cs
                    var     -> QVarSym var : lexer cs

-- vim: ts=4:sts=4:expandtab:ai
