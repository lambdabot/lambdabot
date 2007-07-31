-- Haskell expression parser.  Big hack, but only uses documented APIs so it
-- should be more robust than the previous hack.
module Lib.Parser (parseExpr, ParseResult(..)) where

import Language.Haskell.Syntax
import Language.Haskell.Parser

parseExpr :: String -> ParseResult HsExp
parseExpr s
    | not (balanced 0 s) = ParseFailed (SrcLoc "" 0 0) "Unbalanced parenthesis"
    | otherwise          = case parseModule wrapped of
        ParseOk (HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs e) _]) -> ParseOk e
        ParseFailed a b -> ParseFailed a b
  where
    wrapped = "module Main where { main = (" ++ s ++ ")}"

    balanced :: Int -> String -> Bool
    balanced n ""       = n == 0
    balanced n ('(':cs) =           balanced (n+1) cs
    balanced n (')':cs) = n > 0  && balanced (n-1) cs
    balanced n (_  :cs) =           balanced n     cs
