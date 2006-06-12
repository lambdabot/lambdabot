--
-- | Fetch URL page titles of HTML links.
--
module Plugin.Url (theModule) where

import Plugin

PLUGIN Url

instance Module UrlModule () where
    moduleHelp  _ "url-title"     = "url-title <url>. Fetch the page title."
    moduleCmds  _                 = ["url-title"]
    process_    _ "url-title" url = getPageTitle url
    contextual  _ _ _ text        = case containsUrl text of
                                      Nothing  -> return []
                                      Just url -> getPageTitle url
{-
  DONS
  Can't seem to figure this last part out, GHC keeps complaining about
  readMS but after looking at a few of the other plugins, I'm still
  not sure what is wrong.  Ideally, I'd like to be able to let an
  admin turn the feature on and off.

    modulePrivs _                 = ["url-on", "url-off"]
    moduleDefState _              = return True
    process_    _ "url-on"        = withMS $ \_ k -> k True >> return ["Enabled"]
    process_    _ "url-off"       = withMS $ \_ k -> k False >> return ["Disabled"]
    contextual  _ _ _ text        = do alive <- ReadMS
                                       case alive of 
                                         True  -> case containsUrl text of
                                                    Nothing  -> return []
                                                    Just url -> getPageTitle url
                                         False -> return []
-}

------------------------------------------------------------------------

-- | Searches a string for an embeddded URL and returns it.
containsUrl :: String -> Maybe String
containsUrl text = do
    (_,kind,rest,_) <- matchRegexAll begreg text
    let url = takeWhile (/=' ') rest
    return $ kind ++ url
    where
      begreg = mkRegexWithOpts "https?://"  True False

-- DONS Is there a nicer way to write this function?  My concern is
-- the multiple case statements.
-- | Fetches a page title for the specified URL.
getPageTitle :: String -> LB [String]
getPageTitle url = 
    case parseURI url of
      Nothing    -> return ["You have specified an invalid URL"]
      Just uri   -> do 
        contents <- io $ getHtmlPage uri
        return $ case extractTitle contents of
                   Nothing    -> [] -- Might be non-html content
                   Just title -> ["The title of that page is \""++title++"\""]
-- | Fetch the contents of a URL following HTTP redirects.  It returns
-- a list of strings comprising the server response which includes the
-- status line, response headers, and body.
getHtmlPage :: URI -> IO [String]
getHtmlPage uri = do
    contents <- io $ getURIContents uri
    case responseStatus contents of
      301       -> getHtmlPage $ fromJust $ locationHeader contents
      302       -> getHtmlPage $ fromJust $ locationHeader contents
      _         -> return contents
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
isTextHtml contents 
    | val == "text/html" = True
    | otherwise          = False
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
