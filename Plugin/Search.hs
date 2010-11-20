{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ViewPatterns #-}
-- | Search various things, Wikipedia and google for now.
--
-- (c) 2005 Samuel Bronson
-- (c) 2006 Don Stewart

-- Joel Koerwer 11-01-2005 generalized query for different methods
--   and added extractConversion to make things like @google 1+2 work
module Plugin.Search (theModule) where

import Plugin
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (anyAttr, tagOpen)
import Data.Maybe (listToMaybe)

$(plugin "Search")

engines :: [(String, (URI, String -> String, [Header]))]
engines =
   [("google", (googleUri, (\s -> "?hl=en&q="++s++"&btnI=I'm+Feeling+Lucky"), googleHeaders)),
    -- ("wikipedia", (wikipediaUri, ("?search="++), [])), -- this has changed and Wikipedia requires a User-Agent string
    ("gsite", (googleUri, (\s -> "?hl=en&q=site%3A"++s++"&btnI=I'm+Feeling+Lucky"), googleHeaders)),
    ("gwiki", (googleUri, (\s -> "?hl=en&q=site%3Ahaskell.org/haskellwiki+" ++s++"&btnI=I'm+Feeling+Lucky"), googleHeaders))
   ]

googleHeaders :: [Header]
googleHeaders = [mkHeader HdrReferer "http://www.google.com/"]

normalizeOptions :: NormalizeRequestOptions a
normalizeOptions = defaultNormalizeRequestOptions {
    normDoClose = True,
    normForProxy = isJust (proxy config),
    normUserAgent = Nothing } -- there is a default user agent, perhaps we want it?

makeUri :: String -> String -> URI
makeUri regName path = nullURI {
    uriScheme = "http:",
    uriAuthority = Just (URIAuth { uriUserInfo = "", uriRegName = regName, uriPort = "" }),
    uriPath = path }

googleUri :: URI
googleUri = makeUri "www.google.com" "/search"
-- wikipediaUri = makeUri "en.wikipedia.org" "/wiki/Special:Search"

instance Module SearchModule () where
    moduleHelp _ s      = case s of
         "google"    -> "google <expr>. Search google and show url of first hit"
         -- "wikipedia" -> "wikipedia <expr>. Search wikipedia and show url of first hit"
         "gsite"     -> "gsite <site> <expr>. Search <site> for <expr> using google"
         "gwiki"     -> "gwiki <expr>. Search (new) haskell.org wiki for <expr> using google."
    moduleCmds _ = map fst engines
    process_ _ s e = lift $ searchCmd s (dropSpace e)

------------------------------------------------------------------------

searchCmd :: String -> String -> LB [String]
searchCmd _          []   = return ["Empty search."]
searchCmd engineName (Network.HTTP.urlEncode -> query)
    | engineName == "google" = -- for Google we do both to get conversions, e.g. for '3 lbs in kg'
        doHTTP request $ \response -> 
        case response of
            Response { rspCode = (3,0,2), rspHeaders = (lookupHeader HdrLocation -> Just url) } ->
                doGoogle >>=  handleUrl url
            _ -> fmap (\extra -> if null extra then ["No Result Found."] else extra) doGoogle
    | otherwise = 
        doHTTP request $ \response -> 
        case response of
            Response { rspCode = (3,0,2), rspHeaders = (lookupHeader HdrLocation -> Just url) } ->
                handleUrl url []
            _ -> return ["No Result Found."]
  where handleUrl url extra = do
            title <- io $ runWebReq (urlPageTitle url) (proxy config)
            return $ extra ++ maybe [url] (\t -> [url, t]) title
        Just (uri, makeQuery, headers) = lookup engineName engines
        request = normalizeRequest normalizeOptions $ Request {
            rqURI = uri { uriQuery = makeQuery query },
            rqMethod = HEAD,
            rqHeaders = headers,
            rqBody = "" }
        doGoogle = 
            doHTTP (request { rqMethod = GET, rqURI = uri { uriQuery = "?hl=en&q=" ++ query } }) $ \response ->
                case response of
                    Response { rspCode = (2,_,_), rspBody = (extractConversion -> Just result) } ->
                        return [result]
                    _ -> return []

doHTTP :: HStream a => Request a -> (Response a -> LB [String]) -> LB [String]
doHTTP request handler = do
    result <- io $ simpleHTTP request
    case result of
        Left connError -> return ["Connection error: "++show connError]
        Right response -> handler response

-- This is clearly fragile.
extractConversion :: String -> Maybe String
extractConversion (parseTags -> tags) = listToMaybe [txt |
    section <- sections (tagOpen ("h2"==) (anyAttr (\(name, value) -> name == "class" && value == "r"))) tags,
    txt <- [dropSpace $ drop 1 $ dropWhile (/= '=') t | TagText t <- section],
    not (null txt)]
