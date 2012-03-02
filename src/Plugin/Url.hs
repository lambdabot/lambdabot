{-# LANGUAGE TemplateHaskell, TypeFamilies, PatternGuards #-}
-- | Fetch URL page titles of HTML links.
module Plugin.Url (theModule) where

import Plugin

import qualified Text.Regex as R -- legacy

plugin "Url"

instance Module UrlModule where
    type ModuleState UrlModule = Bool
    
    moduleCmds _ =
        [ (command "url-title")
            { help = say "url-title <url>. Fetch the page title."
            , process = (mapM_ say =<<) . lift . lift 
                . maybe (return ["Url not valid."]) fetchTitle
                . containsUrl
            }
        , (command "tiny-url")
            { help = say "tiny-url <url>. Shorten <url>."
            , process = (mapM_ say =<<) . lift . lift 
                . maybe (return ["Url not valid."]) fetchTiny 
                . containsUrl
            }
        , (command "url-on")
            { privileged = True
            , help = say "url-on: enable automatic URL summaries"
            , process = const $ do
                lift (writeMS True)
                say "Url enabled"
            }
        , (command "url-off")
            { privileged = True
            , help = say "url-off: disable automatic URL summaries"
            , process = const $ do
                lift (writeMS False)
                say "Url disabled"
            }
        ]
    moduleDefState _              = return True -- url on
    moduleSerialize _             = Just stdSerial

    contextual _ text = do
      alive <- lift readMS
      if alive && (not $ areSubstringsOf ignoredStrings text)
        then case containsUrl text of
               Nothing  -> return ()
               Just url
                 | length url > 60 -> do
                     title <- lift (lift (fetchTitle url))
                     tiny  <- lift (lift (fetchTiny  url))
                     mapM_ say $ zipWith' cat title tiny
                 | otherwise -> lift (lift (fetchTitle url)) >>= mapM_ say
        else return ()
      where cat x y = x ++ ", " ++ y
            zipWith' _ [] ys = ys
            zipWith' _ xs [] = xs
            zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

------------------------------------------------------------------------

-- | Fetch the title of the specified URL.
fetchTitle :: String -> LB [String]
fetchTitle url = do
    title <- io $ runWebReq (urlPageTitle url) (proxy config)
    return $ maybe [] return title

-- | base url for fetching tiny urls
tinyurl :: String
tinyurl = "http://tinyurl.com/api-create.php?url="

-- | Fetch the title of the specified URL.
fetchTiny :: String -> LB [String]
fetchTiny url
    | Just uri <- parseURI (tinyurl ++ url) = do
        tiny <- io $ runWebReq (getHtmlPage uri) (proxy config)
        return $ maybe [] return $ findTiny $ foldl' cat "" tiny
    | otherwise = return $ maybe [] return $ Just url
    where cat x y = x ++ " " ++ y

-- | Tries to find the start of a tinyurl
findTiny :: String -> Maybe String
findTiny text = do
  (_,kind,rest,_) <- R.matchRegexAll begreg text
  let url = takeWhile (/=' ') rest
  return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
  where
  begreg = R.mkRegexWithOpts "http://tinyurl.com/" True False

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
     "title of that page",   -- Ignore others like the old me
     urlTitlePrompt]         -- Ignore others like me

-- | Suffixes that should be stripped off when identifying URLs in
-- contextual messages.  These strings may be punctuation in the
-- current sentence vs part of a URL.  Included here is the NUL
-- character as well.
ignoredUrlSuffixes :: [String]
ignoredUrlSuffixes = [".", ",", ";", ")", "\"", "\1", "\n"]

-- | Searches a string for an embeddded URL and returns it.
containsUrl :: String -> Maybe String
containsUrl text = do
    (_,kind,rest,_) <- R.matchRegexAll begreg text
    let url = takeWhile (`notElem` " \n\t\v") rest
    return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
    where
      begreg = R.mkRegexWithOpts "https?://"  True False

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

