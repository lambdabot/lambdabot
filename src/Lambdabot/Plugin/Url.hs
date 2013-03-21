{-# LANGUAGE PatternGuards #-}
-- | Fetch URL page titles of HTML links.
module Lambdabot.Plugin.Url (theModule) where

import Lambdabot.Plugin
import Lambdabot.Util.Browser

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Network.Browser
import Network.HTTP
import Text.Regex.TDFA

theModule :: Module Bool
theModule = newModule
    { moduleCmds = return
        [ (command "url-title")
            { help = say "url-title <url>. Fetch the page title."
            , process =
                  maybe (say "Url not valid.") (mbSay <=< fetchTitle)
                . containsUrl
            }
        , (command "tiny-url")
            { help = say "tiny-url <url>. Shorten <url>."
            , process =
                  maybe (say "Url not valid.") (mbSay <=< fetchTiny)
                . containsUrl
            }
        , (command "url-on")
            { privileged = True
            , help = say "url-on: enable automatic URL summaries"
            , process = const $ do
                writeMS True
                say "Url enabled"
            }
        , (command "url-off")
            { privileged = True
            , help = say "url-off: disable automatic URL summaries"
            , process = const $ do
                writeMS False
                say "Url disabled"
            }
        ]
    , moduleDefState              = return True -- url on
    , moduleSerialize             = Just stdSerial

    , contextual = \text -> do
      alive <- lift readMS
      if alive && (not $ areSubstringsOf ignoredStrings text)
        then case containsUrl text of
               Nothing  -> return ()
               Just url
                 | length url > 60 -> do
                     title <- fetchTitle url
                     tiny  <- fetchTiny  url
                     say (intercalate ", " (catMaybes [title, tiny]))
                 | otherwise -> mbSay =<< fetchTitle url
        else return ()
    }

mbSay :: Maybe String -> Cmd (ModuleT Bool LB) ()
mbSay = maybe (return ()) say

------------------------------------------------------------------------

-- | The string that I prepend to the quoted page title.
urlTitlePrompt :: String
urlTitlePrompt = "Title: "

-- | Fetch the title of the specified URL.
fetchTitle :: MonadLB m => String -> m (Maybe String)
fetchTitle url = fmap (fmap (urlTitlePrompt ++)) (browseLB (urlPageTitle url))

-- | base url for fetching tiny urls
tinyurl :: String
tinyurl = "http://tinyurl.com/api-create.php?url="

-- | Fetch the title of the specified URL.
fetchTiny :: MonadLB m => String -> m (Maybe String)
fetchTiny url = do
    (_, response) <- browseLB (request (getRequest (tinyurl ++ url)))
    case rspCode response of
      (2,0,0) -> return $ findTiny (rspBody response)
      _       -> return Nothing

-- | Tries to find the start of a tinyurl
findTiny :: String -> Maybe String
findTiny text = do
    mr <- matchM begreg text
    let kind = mrMatch mr
        rest = mrAfter mr
        url = takeWhile (/=' ') rest
    return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
    where
        begreg :: Regex
        begreg = makeRegexOpts opts defaultExecOpt "http://tinyurl.com/"
        opts = defaultCompOpt {caseSensitive = False}

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
    mr <- matchM begreg text
    let kind = mrMatch mr
        rest = mrAfter mr
        url = takeWhile (`notElem` " \n\t\v") rest
    return $ stripSuffixes ignoredUrlSuffixes $ kind ++ url
    where
        begreg = makeRegexOpts opts defaultExecOpt "https?://"
        opts = defaultCompOpt { caseSensitive = False }

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
