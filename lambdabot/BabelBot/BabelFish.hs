
module BabelBot.BabelFish(babelFish, shortLangs) where

import MiniHTTP
import BotConfig                (proxy)

import Data.Char (toLower)
import Maybe
import Network.URI
import Control.Monad.Error
import Data.List
import Text.Regex

babelFishURL :: [Char]
babelFishURL = "http://babelfish.altavista.com/babelfish/tr"

supportedLangs :: [([Char], [Char])]
supportedLangs =
    [("german", "de"),
     ("greek", "el"),
     ("english", "en"),
     ("spanish", "es"),
     ("french", "fr"),
     ("italian", "it"),
     ("japanese", "ja"),
     ("korean", "ko"),
     ("dutch", "nl"),
     ("portuguese", "pt"),
     ("russian", "ru"),
     ("chinese-simp", "zh"),
     ("chinese-trad", "zt")]

shortLangs :: [[Char]]
shortLangs = ["de","el","en","es","fr","it","ja","ko","nl","pt","ru","zh","zt"]

type URIVars = [(String, String)]

constantVars :: URIVars
constantVars = 
    [("doit", "done"),
     ("intl", "1"),
     ("tt", "urltext")]

justEither :: a -> Maybe b -> Either a b
justEither a Nothing = Left a
justEither _ (Just v) = Right v

langVars :: String -> String -> URIVars -> Either String URIVars
langVars inLang outLang vars = 
    do
    lookupLang inLang >>= \ins ->
	lookupLang outLang >>= \outs -> 
	    Right (("lp", ins ++ "_" ++ outs) : vars)
    where
    lookupLang lang | length lang == 2 && lang `elem` shortLangs = Right lang
    lookupLang lang = justEither ("Language " ++ lang ++ " not supported") 
                                 (lookup (map toLower lang) supportedLangs) 

mkBody :: String -> String -> String -> Either String String
mkBody inLang outLang string 
    = mapconcat mkString "&" vars
    where
    vars = langVars inLang outLang (("urltext", string) : constantVars)
    mkString (var, val) = var ++ "=" ++ (urlEncode val)
    mapconcat :: (a -> String) -> String -> Either String [a] -> Either String String
    mapconcat _ _ (Left s) = Left s
    mapconcat f m (Right l) = Right $ concat (intersperse m (map f l))

getBabel :: [String] -> String
getBabel lins =  cleanLine (concat (intersperse " " region))
    where
    region = hd ++ [(head tl)]
    (hd, tl) = span (\x -> (matchRegex reEnd x) == Nothing)
	       (dropWhile (\x -> (matchRegex reStart x) == Nothing) lins)

    cleanLine x = maybe "can't parse this language" head $ matchRegex reLine x

    reLine =  mkRegex ".*padding:10px[^>]*>(.*)</div>.*"
    reStart = mkRegex ".*padding:10px[^>]*>"
    reEnd = mkRegex "</div>"
    
babelFish :: String -> String -> String -> IO String
babelFish inLang outLang string = do
    body <- either (\s -> error ("Error: " ++ s)) 
                   (\body -> return body) (mkBody inLang outLang string)
    lins <- readPage proxy uri (mkPost uri body) body
    return (getBabel lins)
    where
    uri = (fromJust $ parseURI babelFishURL)
