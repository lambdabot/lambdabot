module GoogleModule (GoogleModule, googleModule, theModule) where

import IRC
import MiniHTTP

import Data.List (findIndex)
import Control.Monad.State (MonadIO, liftIO)
import Network.URI (parseURI)


newtype GoogleModule = GoogleModule ()

theModule :: MODULE
theModule = MODULE googleModule

googleModule :: GoogleModule
googleModule = GoogleModule ()

instance Module GoogleModule () where
    moduleName   _ = return "google"
    moduleSticky _ = False

    moduleHelp _ s = return $ case s of
                     "google" -> "search google and show first hit"
                     _        -> "module for googling"
    
    commands     _ = return ["google"]
    process _ _ src cmd rest = case cmd of
                               "google" -> googleCmd src rest
                               _        -> error "google: invalid command"

queryUrl   :: String -> String
queryUrl q = "http://www.google.com/search?hl=en&q="
             ++ urlEncode q
             ++ "&btnI=I%27m+Feeling+Lucky"

query   :: String -> IO [String]
query q = readPage Nothing uri request ""
    where url = queryUrl q
          Just uri = parseURI url
          request = ["HEAD " ++ url ++ " HTTP/1.0", ""]

extractLoc         :: [String] -> String
extractLoc headers = case lookup "Location" $ concatMap f headers of
                     Just x  -> x
                     Nothing -> error "No matching header"
    where f str = case findIndex (==':') str of
                  Just n -> [(take n str, drop (n+2) str)]
                  Nothing -> []

googleCmd          :: String -> String -> ModuleT s IRC ()
googleCmd src rest = do result <- liftIO $ query rest
                        ircPrivmsg src (extractLoc $ tail result)


#ifdef TESTING
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

testExtract :: String
testExtract = extractLoc $ tail testHeaders
#endif
