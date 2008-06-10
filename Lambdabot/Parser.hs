{-# LANGUAGE Rank2Types #-}

-- Haskell expression parser.  Big hack, but only uses documented APIs so it
-- should be more robust than the previous hack.
module Lambdabot.Parser (parseExpr, parseDecl, withParsed, prettyPrintInLine) where

import Control.Monad.Error () -- Monad Either instance
import Data.Char
import Data.Generics
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Lambdabot.FixPrecedence

parseExpr :: String -> Either String HsExp
parseExpr s
    | not (balanced 0 ' ' s) = Left "Unbalanced parentheses"
    | otherwise          = case parseModule wrapped of
        ParseOk (HsModule _ _ _ _ [HsPatBind _ _ (HsUnGuardedRhs e) _])
            -> Right $ fixPrecedence $ unparen e
        ParseFailed (SrcLoc _ _ col) msg
            -> Left $ showParseError msg (col - length prefix) s
  where
    prefix  = "module Main where { main = ("
    wrapped = prefix ++ s ++ "\n)}"

    unparen (HsParen e) = e
    unparen e           = e

    -- balanced (open-parentheses) (previous-character) (remaining-string)
    balanced :: Int -> Char -> String -> Bool
    balanced n _ ""           = n == 0
    balanced n _ ('(':cs)     =           balanced (n+1) '(' cs
    balanced n _ (')':cs)     = n > 0  && balanced (n-1) ')' cs
    balanced n p (c  :cs)
      | c `elem` "\"'" && (not  (isAlphaNum p) || c /= '\'')
                              =           balancedString c n cs
    balanced n p ('-':'-':_)
      | not (isSymbol p)      = n == 0
    balanced n _ ('{':'-':cs) =           balancedComment 1 n cs
    balanced n _ (c  :cs)     =           balanced n     c   cs

    balancedString :: Char -> Int -> String -> Bool
    balancedString _     n ""          = n == 0 -- the parse error will be reported by L.H.Parser
    balancedString delim n ('\\':c:cs)
      | isSpace c                      = case dropWhile isSpace cs of
                                            '\\':cs' -> balancedString delim n cs'
                                            cs'      -> balancedString delim n cs'
      | otherwise                      = balancedString delim n cs
    balancedString delim n (c     :cs)
      | delim == c                     = balanced n c cs
      | otherwise                      = balancedString delim n cs

    balancedComment :: Int -> Int -> String -> Bool
    balancedComment 0 n cs           = balanced n ' ' cs
    balancedComment _ _ ""           = True -- the parse error will be reported by L.H.Parser
    balancedComment m n ('{':'-':cs) = balancedComment (m+1) n cs
    balancedComment m n ('-':'}':cs) = balancedComment (m-1) n cs
    balancedComment m n (_      :cs) = balancedComment m     n cs


parseDecl :: String -> Either String HsDecl
parseDecl s = case parseModule s of
        ParseOk (HsModule _ _ _ _ [d])   -> Right $ fixPrecedence d
        ParseFailed (SrcLoc _ _ col) msg -> Left $ showParseError msg col s

showParseError :: String -> Int -> String -> String
showParseError msg col s = " " ++ msg
                       ++ case (col < 0, drop (col - 1) s) of
                            (True, _) -> " at end of input" -- on the next line, which has no prefix
                            (_,[]   ) -> " at end of input"
                            (_,ctx  ) -> let ctx' = takeWhile (/= ' ') ctx
                                         in " at \"" ++ (take 5 ctx')
                                         ++ (if length ctx' > 5 then "..." else "")
                                         ++ "\" (column " ++ show col ++ ")"

-- Not really parsing

withParsed :: (forall a. (Data a, Eq a) => a -> a) -> String -> String
withParsed f s = case (parseExpr s, parseDecl s) of
                    (Right a, _) -> prettyPrintInLine $ f a
                    (_, Right a) -> prettyPrintInLine $ f a
                    (Left e,  _) -> e

prettyPrintInLine :: Pretty a => a -> String
prettyPrintInLine = prettyPrintWithMode (defaultMode { layout = PPInLine })
