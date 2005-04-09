--
-- | Talk to hot chixxors.
--

module VixenModule where

import IRC
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import Control.Monad.State      (MonadIO, liftIO)
import Random
import Control.Exception
import Text.Regex
import Maybe
import GHC.IOBase (unsafePerformIO)
------------------------------------------------------------------------

newtype VixenModule = VixenModule ()

theModule :: MODULE
theModule = MODULE vixenModule

vixenModule :: VixenModule
vixenModule = VixenModule ()

instance Module VixenModule () where
    moduleName   _ = "vixenlove"
    moduleSticky _ = False

    moduleHelp _ s = return $ case s of
             "vixenlove" -> "talk to me, big boy"
             _           -> "sergeant curry's lonely hearts club"
    
    moduleCmds     _ = return ["vixen"]
    process _ _ src cmd rest = case cmd of
               "vixen" -> vixenCmd src rest
               _       -> error "vixen error: i'm just a girl!"


-- ideally, mkResponses state would be cached between calls - the file could be large.
vixenCmd :: String -> String -> ModuleT s IRC ()
vixenCmd src rest = do 
	state <-  liftIO $ readConfig
        result <- liftIO $ vixen (mkResponses state) rest
        ircPrivmsg src result

----------------------------------------------------------------------

-- format
-- top:  (regex, wtree)
-- wtr:  (wtr wtr ... )
--    |   str
readConfig :: IO RespChoice
readConfig = do
  l <- readFile ".vixenrc"
  case (parse parseChoices "vixenlove" l) of
    Left a  -> error $ "Parse error at"++ show a
    Right b -> return (b ++ [(mkRegex ".*", Leaf "If you see this, gentle sir, know that you are being trolled by a poorly configured VixenLove program")])
  
-------------------------------------------------------------------------------
-- Parser


lexer :: P.TokenParser st
lexer = P.makeTokenParser haskellStyle
parens :: CharParser st a
	  -> CharParser st a
parens = P.parens lexer
stringLiteral :: CharParser st
	       String
stringLiteral = P.stringLiteral lexer
lexeme :: CharParser st a
	  -> CharParser st a
lexeme = P.lexeme lexer
lstring :: GenParser Char
							st
							String
lstring = lexeme stringLiteral <?> "lexeme"
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

parseChoices :: GenParser Char
		          st
		          [(Regex, WTree String)]
parseChoices = do
  whiteSpace <?> "whitespace"
  l <- many parseMatch <?> "many parses"
  return [(a,b) | (Just a,b) <- l] 

parseMatch :: GenParser Char
							   st
							   (Maybe Regex,
							    WTree String)
parseMatch = parens (do {
               r <- regexP <?> "r";
               wtr <- many wtreeP <?> "wtr" ; return (r,Node wtr) 
	     }) <?> "parseMatch"

mkRegexMaybe :: String -> Maybe Regex
mkRegexMaybe l = let r = mkRegex l in
		 case unsafePerformIO $ Control.Exception.try (evaluate r) of
		    Left  _  -> Nothing
                    Right s -> Just s
		 
		   
regexP :: GenParser Char
	            st
		    (Maybe Regex)
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

randomWTreeElt :: WTree a -> IO a
randomWTreeElt (Leaf a)  = return a
randomWTreeElt (Node ls) = do
  elt <- randomElt ls
  randomWTreeElt elt

randomElt :: [a] -> IO a
randomElt ls = do
  elt <- randomRIO (1,(length ls))
  return $ ls !! (elt-1)

-- use IO only for random, could remove it.
vixen :: (String -> WTree String) -> String -> IO String
vixen responder them = randomWTreeElt (responder them)

match :: Regex -> String -> Bool
match r s = isJust $ matchRegex r s

mkResponses :: RespChoice -> String -> WTree String
mkResponses choices them = (\((_,wtree):_) -> wtree) $
                           filter (\(reg,_) -> match reg them) choices

