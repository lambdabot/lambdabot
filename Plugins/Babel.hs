--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2004 Simon Winwood - http://www.cse.unsw.edu.au/~sjw
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | A translator module for lambdabot, also provides some quote code.
--
module Plugins.Babel (theModule) where

import Lambdabot
import MiniHTTP
import Config               (proxy,config)

import Data.Maybe      
import Data.Char            (toLower)
import Data.List

import Control.Monad.Error

import Network.URI
import Text.Regex

------------------------------------------------------------------------

newtype BabelModule = BabelModule ()

theModule :: MODULE
theModule = MODULE $ BabelModule ()

instance Module BabelModule () where

        moduleCmds _ = ["babel"]
        moduleHelp _ "babel"    = concat ["usage: babel lang lang phrase"]
        process_ _ "babel"     s = run_babel s

        {-
        -- totally unrelated :}
        process _ _ src "timein" s =
          if s == "help"
            then ircPrivmsg src "  http://www.timeanddate.com"
            else do (o,_,_) <- liftIO $ popen "timein" [s] Nothing
                    ircPrivmsg src $ "  " ++ o
        -}

--
-- The @babel command.
--
--      * translate a phrase
--      * translate last 'i' lines of history buffer
--      * translate a line of history indexed by a regex
--
-- TODO add range/context. i.e. !f-3 or 5-4
--
run_babel :: String -> LB [String]
run_babel s = do
        let cmd = split ' ' 3 s
        msg <- run_babel' cmd
        let msg' = map ("  " ++) msg
        return [unlines msg']

-- help msg
run_babel' :: (MonadIO m) => [String] -> m [String]
run_babel' ["help"]      = return $ ["usage: babel lang lang phrase"]
run_babel' ["languages"] = return $ [show shortLangs]
run_babel' [f,t,i]       = do p <- liftIO $ babelFish f t i ; return [p]
run_babel' _             = return ["bzzt."]

------------------------------------------------------------------------

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
    lins <- readPage (proxy config) uri (mkPost uri body) body
    return (getBabel lins)
    where
    uri = (fromJust $ parseURI babelFishURL)


------------------------------------------------------------------------

split :: Char -> Int -> String -> [String]
split c i s =
        let fn 0 t = t:[]
            fn j t = let (xs,ys) = break (== c) t
                     in case ys of
                        [] -> xs:[]
                        _  -> xs: fn (j-1) (tail ys)
        in fn (i-1) s
