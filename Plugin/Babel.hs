{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- Copyright (c) 2004 Simon Winwood - http://www.cse.unsw.edu.au/~sjw
-- Copyright (c) 2004-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | A translator module for lambdabot, a binding to babelfish
module Plugin.Babel where

import Plugin
import qualified Text.Regex as R

$(plugin "Babel")

instance Module BabelModule () where
    moduleCmds _   = ["babel"]
    process_   _ _ = lift . babel
    moduleHelp _ _ = unlines ["babel <lang1> <lang2> <phrase>."
                             ,"Translate a phrase in lang1 to lang2."
                             ,"Language is an element of" ++ showClean supportedLangs]

--
-- The @babel command.
--
--      * translate a phrase
--      * translate last 'i' lines of history buffer
--      * translate a line of history indexed by a regex
--
-- TODO add range/context. i.e. !f-3 or 5-4
--
babel :: String -> LB [String]
babel s = do
    let cmd = split2 ' ' 3 s
    msg <- babel' cmd
    let msg' = map ("  " ++) msg
    return [unlines msg']

-- help msg
babel' :: (MonadIO m) => [String] -> m [String]
babel' ["help"]      = return $ ["usage: babel lang lang phrase"]
babel' ["languages"] = return $ [show shortLangs]
babel' [f,t,i]       = do p <- liftIO $ babelFish f t i ; return [p]
babel' _             = return ["bzzt."]

------------------------------------------------------------------------

babelFishURL :: [Char]
babelFishURL = "http://babelfish.altavista.com/babelfish/tr"

supportedLangs :: [([Char], [Char])]
supportedLangs =
    [("german", "de")
    ,("greek", "el")
    ,("english", "en")
    ,("spanish", "es")
    ,("french", "fr")
    ,("italian", "it")
    ,("dutch", "nl")
    ,("portuguese", "pt")
--   ("japanese", "ja")
--   ("korean", "ko")
--   ("russian", "ru")
--   ("chinese-simp", "zh")
--   ("chinese-trad", "zt")
     ]

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
    (hd, tl) = span (\x -> (not (matches' reEnd x)))
                    (dropWhile (\x -> (not (matches' reStart x))) lins)

    cleanLine x = maybe "can't parse this language" head $ R.matchRegex reLine x

    reLine  = regex' ".*padding:10px[^>]*>(.*)</div>.*"
    reStart = regex' ".*padding:10px[^>]*>"
    reEnd   = regex' "</div>"

babelFish :: String -> String -> String -> IO String
babelFish inLang outLang string = do
    body <- either (\s -> error ("Error: " ++ s))
                   (\body -> return body) (mkBody inLang outLang string)
    lins <- readPage (proxy config) uri (mkPost uri body) body
    return (getBabel lins)
    where
    uri = (fromJust $ parseURI babelFishURL)

--------------------------------------------------------------

        {-
        -- totally unrelated :}
        process _ _ src "timein.hs" s =
          if s == "help"
            then ircPrivmsg src "  http://www.timeanddate.com"
            else do (o,_,_) <- liftIO $ popen "timein.hs" [s] Nothing
                    ircPrivmsg src $ "  " ++ o
        -}
