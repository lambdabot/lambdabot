{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards #-}
-- | DICT (RFC 2229) Lookup Module for lambdabot IRC robot.
-- Tom Moertel <tom@moertel.com>
module Plugin.Dict (theModule) where

import Plugin
import qualified Plugin.Dict.DictLookup as Dict

$(plugin "Dict")

-- | This is the module handler.  Here we process commands from users.

instance Module DictModule () where
    moduleHelp _ _ = getHelp []
    moduleCmds _   = "dict" : "dict-help" : dictNames

    process_ _ "dict"      _    = return [quickHelp]
    process_ _ "dict-help" rest = return [getHelp (words rest)]
    process_ _ cmd         rest = do
        let s = parseTerms rest
        results <- mapM doLookup s
        return $ case results of
            [] -> []
            _  -> [concat results]
      where
        doLookup w = io $ do
            result <- lookupFn w
            return $ either ("Error: " ++) id result

        lookupFn = uncurry Dict.simpleDictLookup . fst $
                   fromJust (lookup cmd dictTable)

-- | Configuration.

dictTable :: [(String, ((Dict.QueryConfig, String), String))]
dictTable =
    -- @command   ((server  , database),    description)
    [ ("all-dicts",((dict_org, "*")       , "Query all databases on dict.org"))
    , ("elements", ((dict_org, "elements"), "Elements database"))
    , ("web1913" , ((dict_org, "web1913"),
          "Webster's Revised Unabridged Dictionary (1913)"))
    , ("wn"      , ((dict_org, "wn"),       "WordNet (r) 1.7"))
    , ("gazetteer",((dict_org, "gazetteer"),"U.S. Gazetteer (1990)"))
    , ("jargon"  , ((dict_org, "jargon"),   "Jargon File"))
    , ("foldoc"  , ((dict_org, "foldoc"),
          "The Free On-line Dictionary of Computing"))
    , ("easton"  , ((dict_org, "easton"),   "Easton's 1897 Bible Dictionary"))
    , ("hitchcock",((dict_org, "hitchcock"),
          "Hitchcock's Bible Names Dictionary (late 1800's)"))
    , ("devils"  , ((dict_org, "devils"),   "The Devil's Dictionary"))
    , ("world02" , ((dict_org, "world02"),  "CIA World Factbook 2002"))
    , ("vera"    , ((dict_org, "vera"),
           "V.E.R.A.: Virtual Entity of Relevant Acronyms"))
--  , ("prelude" , ((moertel_com, "prelude"), "Haskell Standard Prelude"))
    , ("lojban"  , ((lojban_org, "lojban"), "Search lojban.org"))
    ]
    where
    dict_org    = Dict.QC "dict.org" 2628
--  moertel_com = Dict.QC "dict.moertel.com" 2628
    lojban_org  = Dict.QC "lojban.org" 2628

dictNames :: [String]
dictNames = sort (map fst dictTable)


-- | Print out help.

quickHelp :: String
quickHelp = unlines [ "Supported dictionary-lookup commands:"
                    , "  " ++ concatWith " " dictNames
                    , "Use \"dict-help [cmd...]\" for more."
                    ]

getHelp :: [String] -> String
getHelp []    = "I perform dictionary lookups via the following "
              ++ show (length dictNames) ++ " commands:\n"
              ++ (getHelp dictNames)

getHelp dicts = unlines . map gH $ dicts
    where
    gH dict | Just (_, descr) <- lookup dict dictTable
            = pad dict ++ " " ++ descr

            | otherwise
            = "There is no dictionary database '" ++ dict ++ "'."

    pad xs = take padWidth (xs ++ " " ++ repeat '.')
    padWidth = maximum (map length dictNames) + 4


-- | Break a string into dictionary-query terms, handling quoting and
-- escaping along the way.  (This is ugly, and I don't particularly
-- like it.)  Given a string like the following, we want to do the
-- right thing, which is to break it into five query strings:
--
--     firefly "c'est la vie" 'pound cake' 'rock n\' roll' et\ al
--
--     (1) firefly
--     (2) "c'est la vie"
--     (3) 'pound cake'
--     (4) 'rock n\' roll'
--     (5) et\ al

parseTerms :: String -> [String]
parseTerms = pW . words
    where
    pW []  = []
    pW (w@(f:_):ws)
        | f `elem` "'\"" = concatWith " " qws : pW ws'
        | last w == '\\' = let (w':rest) = pW ws in concatWith " " [w, w'] : rest
        | otherwise      = w : pW ws
        where
        (qws, ws') = case break isCloseQuotedWord (w:ws) of
            (qws', [])    -> (init qws' ++ [last qws' ++ [f]], [])
            (qw, w':rest) -> (qw ++ [w'], rest)
        isCloseQuotedWord xs = case reverse xs of
            x:y:_ -> f == x && y /= '\\' -- quote doesn't count if escaped
            x:_   -> f == x
            _     -> False
    pW _ = error "DictModule: parseTerms: can't parse"

