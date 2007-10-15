-- Haskell expression parser.  Big hack, but only uses documented APIs so it
-- should be more robust than the previous hack.
module Lib.Parser (parseExpr, parseDecl, withParsed, prettyPrintInLine) where

import Control.Monad.Error () -- Monad Either instance
import Data.Char
import Data.Generics
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Lib.FixPrecedence

parseExpr :: String -> Either String HsExp
parseExpr s
    | not (balanced 0 s) = Left "Unbalanced parenthesis"
    | otherwise          = case parseModule wrapped of
        ParseOk (HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs e) _])
            -> Right $ fixPrecedence $ unparen e
        ParseFailed (SrcLoc _ _ col) msg
            -> Left $ show (col - length prefix) ++ ": " ++ msg
  where
    prefix  = "module Main where { main = ("
    wrapped = prefix ++ s ++ "\n)}"
    
    unparen (HsParen e) = e
    unparen e           = e
    
    balanced :: Int -> String -> Bool
    balanced n ""       = n == 0
    balanced n ('(':cs) =           balanced (n+1) cs
    balanced n (')':cs) = n > 0  && balanced (n-1) cs
    balanced n (c  :cs) | c `elem` "\"'"
                        =           balancedString c n cs
    balanced n (c:'\'':cs) | isAlphaNum c
                        =           balanced n     cs
    balanced n (_  :cs) =           balanced n     cs
    
    balancedString :: Char -> Int -> String -> Bool
    balancedString delim n ('\\':c:cs)
      | isSpace c                      = case dropWhile isSpace cs of
                                            '\\':cs' -> balancedString delim n cs'
                                            cs'      -> balancedString delim n cs'
      | otherwise                      = balancedString delim n cs
    balancedString delim n (c     :cs)
      | delim == c                     = balanced n cs
      | otherwise                      = balancedString delim n cs


parseDecl :: String -> Either String HsDecl
parseDecl s = case parseModule s of
        ParseOk (HsModule _ _ _ _ [d])   -> Right $ fixPrecedence d
        ParseFailed (SrcLoc _ _ col) msg -> Left $ show col ++ ": " ++ msg

-- Not really parsing

withParsed :: (forall a. (Data a, Eq a) => a -> a) -> String -> String
withParsed f s = case (parseExpr s, parseDecl s) of
                    (Right a, _) -> prettyPrintInLine $ f a
                    (_, Right a) -> prettyPrintInLine $ f a
                    (Left e,  _) -> e

prettyPrintInLine :: Pretty a => a -> String
prettyPrintInLine = prettyPrintWithMode (defaultMode { layout = PPInLine })
