--
-- BSD licence, author Mark Wotton (mwotton@gmail.com)
--
-- | Talk to hot chixxors.
--

module Vixen (mkVixen) where

import Util (stdGetRandItem)

import Data.Maybe (isJust)
import Control.Exception (try, evaluate)

import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import Text.Regex

import GHC.IOBase (unsafePerformIO)


mkVixen :: String -> String -> IO String
mkVixen phraseBook question = do 
  vixen (mkResponses $ readConfig phraseBook) question
----------------------------------------------------------------------

-- format
-- top:  (regex, wtree)
-- wtr:  (wtr wtr ... )
--    |   str
readConfig :: String -> RespChoice
readConfig l = do
  case parse parseChoices "vixenlove" l of
    Left a  -> error $ "Parse error at"++ show a
    Right b -> (b ++ [(mkRegex ".*", Leaf "If you see this, gentle sir, know that you are being trolled by a poorly configured VixenLove program")])
  
-- ----------------------------------------------------------------------------
-- Parser

lexer :: P.TokenParser st
lexer = P.makeTokenParser haskellStyle

parens :: CharParser st a -> CharParser st a
parens = P.parens lexer

stringLiteral :: CharParser st String
stringLiteral = P.stringLiteral lexer

lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

lstring :: GenParser Char st String
lstring = lexeme stringLiteral <?> "lexeme"

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

parseChoices :: GenParser Char st [(Regex, WTree String)]
parseChoices = do
  whiteSpace <?> "whitespace"
  l <- many parseMatch <?> "many parses"
  return [(a,b) | (Just a,b) <- l] 

parseMatch :: GenParser Char st (Maybe Regex, WTree String)
parseMatch = parens (do {
               r <- regexP <?> "r";
               wtr <- many wtreeP <?> "wtr" ; return (r,Node wtr) 
	     }) <?> "parseMatch"

-- case insensitive match
mkRegexMaybe :: String -> Maybe Regex
mkRegexMaybe l = let r = mkRegexWithOpts l True False in 
		 case unsafePerformIO $ Control.Exception.try (evaluate r) of
		    Left  _  -> Nothing
                    Right s -> Just s
		 
		   
regexP :: GenParser Char st (Maybe Regex)
regexP = do { l <- lstring <?> "lstring"; return $ mkRegexMaybe l } <?> "regexP"

wtreeP :: CharParser st (WTree String)
wtreeP = lexeme (
      (parens (do {wtrs <- many wtreeP; return $ Node wtrs}))
  <|> (do l <- lstring; return $ Leaf l)
  <?> "wtree"
  )	  

-- idea is that everything on a given level is 
-- equally likely. Probably enough, could add a
-- weighting if necessary.
--
-- must be one element at least - we guarantee that
-- we can return something from a WTree, so a null l
-- leaf is inappropriate

data WTree a = Leaf a | Node [WTree a]
  deriving Show

type RespChoice = [(Regex, WTree String)]

-- use IO only for random, could remove it.
vixen :: (String -> WTree String) -> String -> IO String
vixen responder them = randomWTreeElt (responder them)

randomWTreeElt :: WTree a -> IO a
randomWTreeElt (Leaf a)  = return a
randomWTreeElt (Node ls) = do
  elt <- stdGetRandItem ls
  randomWTreeElt elt

match :: Regex -> String -> Bool
match r s = isJust $ matchRegex r s

mkResponses :: RespChoice -> String -> WTree String
mkResponses choices them = (\((_,wtree):_) -> wtree) $
                           filter (\(reg,_) -> match reg them) choices

