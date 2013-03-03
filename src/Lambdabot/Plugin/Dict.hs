{-# LANGUAGE PatternGuards #-}
-- | DICT (RFC 2229) Lookup Module for lambdabot IRC robot.
-- Tom Moertel <tom@moertel.com>
module Lambdabot.Plugin.Dict (theModule) where

import Lambdabot.Plugin
import qualified Lambdabot.Plugin.Dict.DictLookup as Dict

import Control.Monad
import Data.List

type Dict = ModuleT () LB

theModule :: Module ()
theModule = newModule
    { moduleCmds = return $
        [ (command "dict-help")
            { help = getHelp []
            , process = getHelp . words
            }
        ] ++
        [ (command name)
            { help = getHelp [name]
            , process = \args -> case parseTerms args of
                [] -> getHelp [name]
                s  -> mapM_ (doLookup >=> sayResult) s
            }
        | (name, (srv, db, _)) <- dictTable
        , let doLookup  = io . Dict.simpleDictLookup srv db
              sayResult = say . either ("Error: " ++) id
        ]
    }

-- | Configuration.

dictTable :: [(String, (Dict.QueryConfig, String, String))]
dictTable =
    -- @command     (server  , database,       description)
    [ ("all-dicts", (dict_org, "*"       ,     "Query all databases on dict.org"))
    , ("bouvier"  , (dict_org, "bouvier",      "Bouvier's Law Dictionary"))
    , ("cide"     , (dict_org, "gcide",        "The Collaborative International Dictionary of English"))
    , ("devils"   , (dict_org, "devil",        "The Devil's Dictionary"))
    , ("easton"   , (dict_org, "easton",       "Easton's 1897 Bible Dictionary"))
    , ("elements" , (dict_org, "elements",     "Elements database"))
    , ("foldoc"   , (dict_org, "foldoc",       "The Free On-line Dictionary of Computing"))
    , ("gazetteer", (dict_org, "gaz2k-places", "U.S. Gazetteer (2000)"))
    , ("hitchcock", (dict_org, "hitchcock",    "Hitchcock's Bible Names Dictionary (late 1800's)"))
    , ("jargon"   , (dict_org, "jargon",       "Jargon File"))
    , ("thesaurus", (dict_org, "moby-thes",    "Moby Thesaurus II"))
    , ("vera"     , (dict_org, "vera",         "V.E.R.A.: Virtual Entity of Relevant Acronyms"))
    , ("wn"       , (dict_org, "wn",           "WordNet (r) 1.7"))
    , ("world02"  , (dict_org, "world02",      "CIA World Factbook 2002"))
    ]
    where
    dict_org    = Dict.QC "dict.org" 2628

dictNames :: [String]
dictNames = sort (map fst dictTable)


-- | Print out help.

getHelp :: [String] -> Cmd Dict ()
getHelp []    = do
    say ("I perform dictionary lookups via the following "
          ++ show (length dictNames) ++ " commands:\n")
    getHelp dictNames

getHelp dicts = mapM_ (say . gH) dicts
    where
    gH dict | Just (_, _, descr) <- lookup dict dictTable
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
        | f `elem` "'\"" = intercalate " " qws : pW ws'
        | last w == '\\' = let (w':rest) = pW ws in intercalate " " [w, w'] : rest
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

