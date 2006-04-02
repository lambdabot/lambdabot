--
-- | Search various things, Wikipedia and google for now.
--
-- (c) 2005 Samuel Bronson
--
-- Joel Koerwer 11-01-2005 generalized query for different methods
--   and added extractConversion to make things like @google 1+2 work
--
module Plugin.Search (theModule) where

import Plugin
import Control.Monad            (mplus)

newtype SearchModule = SearchModule ()

theModule :: MODULE
theModule = MODULE $ SearchModule ()

engines :: [(String, (String, String))]
engines =  [("google"
           ,("http://www.google.com/search?hl=en&q=","&btnI=I%27m+Feeling+Lucky")),

            ("wikipedia"
           ,("http://en.wikipedia.org/wiki/Special:Search?search=", "")),

            ("gsite"
           ,("http://www.google.ca/search?hl=en&q=site%3A", "&btnI=I%27m+Feeling+Lucky"))
           ]

instance Module SearchModule () where
    moduleHelp _ s      = case s of
         "google"    -> "google <expr>. Search google and show url of first hit"
         "wikipedia" -> "wikipedia <expr>. Search wikipedia and show url of first hit"
         "gsite"     -> "gsite <site> <expr>. Search <site> for <expr> using google"
    moduleCmds      _   = map fst engines
    process_ _          = (. dropSpace) . searchCmd

------------------------------------------------------------------------

searchCmd :: String -> String -> LB [String]
searchCmd _ []        = return ["Empty search."]
searchCmd engine rest = do
    headers <- io $ queryit "HEAD" engine rest
    body    <- io $ queryit "GET" engine rest
    return [fromMaybe "No Result Found." $
                extractLoc headers `mplus` extractConversion body] -- ?

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


extractLoc :: [String] -> Maybe String
extractLoc [] = error "No response, something weird is up."
extractLoc (_:headers) = lookup "Location" $ concatMap f headers
        where f s = case findIndex (==':') s of
                          Just n  -> [(take n s, drop (n+2) s)]
                          Nothing -> []

extractConversion :: [String] -> Maybe String
extractConversion [] = error "conv: No response, something weird is up."
extractConversion ls = let
    regex1 = mkRegex "<font size=\\+1><b>"
    regex2 = mkRegex "</b>"
    getConv a = do
        (_,_,s,_) <- matchRegexAll regex1 a
        (s',_,_,_) <- matchRegexAll regex2 s
        return s'

    searchAndReplace new regex = \s -> subRegex (mkRegex regex) s new
    replaceFuncs = zipWith searchAndReplace
                        [    "^",       "",      "x",                      ","]
                        ["<sup>", "</sup>", "&#215;", "<font size=-2> </font>"]
    pipeline [] a = a
    pipeline (f:fs) a = pipeline fs $ f a
    in
    (getConv $ last ls) >>= return . pipeline replaceFuncs

-- ---------------------------------------------------------------------
-- Testing only

{-
testHeaders :: [String]
testHeaders =
    ["HTTP/1.0 302 Found\r",
     "Location: http://sf.net/projects/haskell-libs\r",
     "Cache-Control: private\r",
     "Set-Cookie: PREF=ID=702ec464c1b22b43:TM=1112828216:LM=1112828216"
     ++ ":S=70y-S8HgcsMkVXWx; expires=Sun, 17-Jan-2038 19:14:07 GMT"
     ++ "; path=/; domain=.google.com\r",
     "Content-Type: text/html\r",
     "Server: GWS/2.1\r",
     "Content-Length: 165\r",
     "Date: Wed, 06 Apr 2005 22:56:56 GMT\r",
     "Connection: Keep-Alive\r",
     "\r"]
-}
