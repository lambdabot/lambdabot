{-# OPTIONS_GHC -F -pgmF BotPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Search various things, Wikipedia and google for now.
--
-- (c) 2005 Samuel Bronson
-- (c) 2006 Don Stewart

-- Joel Koerwer 11-01-2005 generalized query for different methods
--   and added extractConversion to make things like @google 1+2 work
module Plugin.Search (theModule) where

import Plugin
import Control.Monad            (mplus)
import qualified Text.Regex as R

PLUGIN Search

engines :: [(String, (String, String))]
engines =
   [("google"
   ,("http://www.google.com/search?hl=en&q=","&btnI=I%27m+Feeling+Lucky")),

    ("wikipedia"
   ,("http://en.wikipedia.org/wiki/Special:Search?search=", "")),

    ("gsite"
   ,("http://www.google.com/search?hl=en&q=site%3A", "&btnI=I%27m+Feeling+Lucky"))
   ]

instance Module SearchModule () where
    moduleHelp _ s      = case s of
         "google"    -> "google <expr>. Search google and show url of first hit"
         "wikipedia" -> "wikipedia <expr>. Search wikipedia and show url of first hit"
         "gsite"     -> "gsite <site> <expr>. Search <site> for <expr> using google"
         "gwiki"     -> "wiki <expr>. Search (new) haskell.org wiki for <expr> using google."
    moduleCmds      _   = "gwiki" : map fst engines
    process_ _ "gwiki" e = lift $ ((. dropSpace) . searchCmd) "gsite" ("haskell.org/haskellwiki" ++ e)
    process_ _ s       e = lift $ ((. dropSpace) . searchCmd) s e

------------------------------------------------------------------------

searchCmd :: String -> String -> LB [String]
searchCmd _ []        = return ["Empty search."]
searchCmd engine rest = do
    headers <- io $ queryit "HEAD" engine rest
    body    <- io $ queryit "GET" engine rest
    case getHeader "Location" headers `mplus` extractConversion body of
      Just url -> do
        title <- io $ runWebReq (urlPageTitle url) (proxy config)
        return $ maybe [url] (\t -> [url, t]) title
      Nothing  -> return ["No Result Found."]

queryUrl :: String -> String -> String
queryUrl engine q = prefix ++ urlEncode q ++ suffix
    where
    (prefix, suffix) = fromMaybe (error "search: invalid command")
                                 (lookup engine engines)

queryit :: String -> String -> String -> IO [String]
queryit meth engine q = readPage (proxy config) uri request ""
    where url = queryUrl engine q
          Just uri = parseURI url
          abs_path = uriPath uri ++ uriQuery uri ++ uriFragment uri
          request  = case proxy config of
                        Nothing -> [meth ++ " " ++ abs_path ++ " HTTP/1.0", ""]
                        _       -> [meth ++ " " ++ url ++ " HTTP/1.0", ""]

extractConversion :: [String] -> Maybe String
extractConversion [] = error "conv: No response, something weird is up."
extractConversion ls = (getConv $ last ls) >>= return . pipeline replaceFuncs
    where
        regex1 = regex' "<font size=\\+1><b>"
        regex2 = regex' "</b>"

        getConv a = do
            (_,_,s,_)  <- R.matchRegexAll regex1 a
            (s',_,_,_) <- R.matchRegexAll regex2 s
            return s'

        searchAndReplace new re = \s -> R.subRegex (regex' re) s new
        replaceFuncs = zipWith searchAndReplace
                            [    "^",       "",      "x",                      ","]
                            ["<sup>", "</sup>", "&#215;", "<font size=-2> </font>"]

        pipeline [] a = a
        pipeline (f:fs) a = pipeline fs $ f a
