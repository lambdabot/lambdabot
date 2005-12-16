--
-- | Search various things, Wikipedia and google for now.
--
-- (c) 2005 Samuel Bronson
--
module Plugins.Search (theModule) where

import Util
import Lambdabot
import Config                   (proxy, config)
import MiniHTTP

import Data.Maybe               (fromMaybe)
import Data.List                (findIndex)
import Control.Monad.State      (MonadIO, liftIO)
import Network.URI              (parseURI)

------------------------------------------------------------------------

newtype SearchModule = SearchModule ()

theModule :: MODULE
theModule = MODULE $ SearchModule ()

engines :: [(String, (String, String))]
engines =  [("google",    ("http://www.google.com/search?hl=en&q=",
                           "&btnI=I%27m+Feeling+Lucky")),
            ("wikipedia", ("http://en.wikipedia.org/wiki/Special:Search?search=", ""))]

instance Module SearchModule () where
    moduleHelp _ s = case s of
         "google"    -> "@google <expr>, search google and show url of first hit"
         "wikipedia" -> "@wikipedia <expr>, search wikipedia and show url of first hit"
         _           -> "module for doing searches"

    moduleCmds   _ = map fst engines
    process _ _ src cmd rest = searchCmd cmd src (dropSpace rest)

------------------------------------------------------------------------

searchCmd :: String -> String -> String -> LB ()
searchCmd _ src [] = ircPrivmsg src "Empty search."
searchCmd engine src rest = do
        result <- liftIO $ query engine rest
        ircPrivmsg' src (extractLoc result)

queryUrl :: String -> String -> String
queryUrl engine q = prefix ++ urlEncode q ++ suffix
    where
    (prefix, suffix) = fromMaybe (error "search: invalid command")
                                 (lookup engine engines)

query :: String -> String -> IO [String]
query engine q = readPage (proxy config) uri request ""
    where url = queryUrl engine q
          Just uri = parseURI url
          request = ["HEAD " ++ url ++ " HTTP/1.0", ""]

extractLoc :: [String] -> String
extractLoc [] = error "No response, something weird is up."
extractLoc (_:headers) =
        fromMaybe "No result found."
                  (lookup "Location" $ concatMap f headers)

        where f s = case findIndex (==':') s of
                          Just n  -> [(take n s, drop (n+2) s)]
                          Nothing -> []

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
