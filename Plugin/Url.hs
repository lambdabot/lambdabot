--
-- | Fetch URL page titles of HTML links.
--
module Plugin.Url (theModule) where

import Plugin

PLUGIN Url

instance Module UrlModule Bool where
    moduleHelp  _ "url-title"     = "url-title <url>. Fetch the page title."
    moduleCmds  _                 = ["url-title"]
    modulePrivs _                 = ["url-on", "url-off"]
    moduleDefState _              = return True -- url on

    process_    _ "url-title" url = getPageTitle url
    process_    _ "url-on"    _   = writeMS True  >> return ["Url enabled"]
    process_    _ "url-off"   _   = writeMS False >> return ["Url disabled"]

    contextual  _ _ _ text        = do 
      alive <- readMS
      if alive && (not $ areSubstringsOf ignoredStrings text)
        then case containsUrl text of
               Nothing  -> return []
               Just url -> getPageTitle url
        else return []

------------------------------------------------------------------------

-- | List of strings that, if present in a contextual message, will
-- prevent the looking up of titles.  This list can be used to stop 
-- responses to lisppaste for example.  Another important use is to
-- another lambdabot looking up a url title that contains another 
-- url in it (infinite loop).  Ideally, this list could be added to 
-- by an admin via a privileged command (TODO).
ignoredStrings :: [String]
ignoredStrings = 
    ["paste",                -- Ignore lisppaste, rafb.net
     "cpp.sourcforge.net",   -- C++ paste bin
     "HaskellIrcPastePage",  -- Ignore paste page
     "title of that page"]   -- Ignore others like me

-- | Limit the maximum title length to prevent jokers from spamming
-- the channel with specially crafted HTML pages.
maxTitleLength :: Int
maxTitleLength = 80

-- | Suffixes that should be stripped off when identifying URLs in
-- contextual messages.  These strings may be punctuation in the
-- current sentence vs part of a URL.  Included here is the NUL
-- character as well.
ignoredUrlSuffixes :: [String]
ignoredUrlSuffixes = [".", ",", ";", ")", "\"", "\1"]

-- | Searches a string for an embeddded URL and returns it.
containsUrl :: String -> Maybe String
containsUrl text = do
    (_,kind,rest,_) <- matchRegexAll begreg text
    let url = takeWhile (/=' ') rest
    return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
    where
      begreg = mkRegexWithOpts "https?://"  True False

-- | Fetches a page title for the specified URL.
getPageTitle :: String -> LB [String]
getPageTitle url
    | Just uri <- parseURI url  = do
        contents <- io $ getHtmlPage uri
        return $ maybe []   -- may be non html
                       (return . 
                        ("The title of that page is \"" ++) . 
                        (++ "\"") .
                        limitLength)
                       (extractTitle contents)

    | otherwise = return ["You have specified an invalid URL"]
    where
      limitLength s 
          | length s > maxTitleLength = (take maxTitleLength s) ++ " ..."
          | otherwise                 = s

-- | Fetch the contents of a URL following HTTP redirects.  It returns
-- a list of strings comprising the server response which includes the
-- status line, response headers, and body.
getHtmlPage :: URI -> IO [String]
getHtmlPage uri = do
    contents <- io $ getURIContents uri
    case responseStatus contents of
      301       -> getHtmlPage $ fromJust $ locationHeader contents
      302       -> getHtmlPage $ fromJust $ locationHeader contents
      200       -> return contents
      _         -> return []
    where
      -- | Parse the HTTP response code from a line in the following
      -- format: HTTP/1.1 200 Success.
      responseStatus hdrs = (read . (!!1) . words . (!!0)) hdrs :: Int

      -- | Return the value of the "Location" header in the server
      -- response 
      locationHeader hdrs = do
        header <- getHeader "Location" hdrs
        locuri <- parseURI header
        return locuri

-- | Fetch the contents of a URL returning a list of strings
-- comprising the server response which includes the status line,
-- response headers, and body.
getURIContents :: URI -> IO [String]
getURIContents uri = readPage (proxy config) uri request ""
    where
      request  = case proxy config of
                   Nothing -> ["GET " ++ abs_path ++ " HTTP/1.0", ""]
                   _       -> ["GET " ++ show uri ++ " HTTP/1.0", ""]

      abs_path = case uriPath uri ++ uriQuery uri ++ uriFragment uri of
                   url@('/':_) -> url
                   url         -> '/':url

-- | Given a server response (list of Strings), return the text in
-- between the title HTML element, only if it is text/html content.
-- TODO: need to decode character entities (or at least the most
-- common ones)
extractTitle :: [String] -> Maybe String
extractTitle contents
    | isTextHtml contents = getTitle $ unlines contents
    | otherwise           = Nothing
    where
      begreg = mkRegexWithOpts "<title> *"  True False
      endreg = mkRegexWithOpts " *</title>" True False

      getTitle text = do
        (_,_,start,_) <- matchRegexAll begreg text
        (title,_,_,_) <- matchRegexAll endreg start
        return $ (unwords . words) title

-- | Is the server response of type "text/html"?
isTextHtml :: [String] -> Bool
isTextHtml []       = False
isTextHtml contents = val == "text/html"
    where
      val        = takeWhile (/=';') ctype
      Just ctype = getHeader "Content-Type" contents

-- | Retrieve the specified header from the server response being
-- careful to strip the trailing carriage return.  I swiped this code
-- from Search.hs, but had to modify it because it was not properly
-- stripping off the trailing CR (must not have manifested itself as a 
-- bug in that code; however, parseURI will fail against CR-terminated
-- strings.
getHeader :: String -> [String] -> Maybe String
getHeader _   []     = Nothing
getHeader hdr (_:hs) = lookup hdr $ concatMap mkassoc hs
    where
      removeCR   = takeWhile (/='\r')
      mkassoc s  = case findIndex (==':') s of
                    Just n  -> [(take n s, removeCR $ drop (n+2) s)]
                    Nothing -> []

-- | Utility function to remove potential suffixes from a string.
-- Note, once a suffix is found, it is stripped and returned, no other
-- suffixes are searched for at that point.
stripSuffixes :: [String] -> String -> String
stripSuffixes []   str   = str
stripSuffixes (s:ss) str
    | isSuffixOf s str   = take (length str - length s) $ str
    | otherwise          = stripSuffixes ss str


-- | Utility function to check of any of the Strings in the specified
-- list are substrings of the String.
areSubstringsOf :: [String] -> String -> Bool 
areSubstringsOf = flip (any . flip isSubstringOf)
    where
      isSubstringOf s str = any (isPrefixOf s) (tails str)

