--
-- | Search Wikipedia.
--

module WikipediaModule (theModule) where

import IRC
import Config                   (proxy, config)
import MiniHTTP

import Data.Maybe               (fromMaybe)
import Data.List                (findIndex)
import Control.Monad.State      (MonadIO, liftIO)
import Network.URI              (parseURI)

------------------------------------------------------------------------

newtype WikipediaModule = WikipediaModule ()

theModule :: MODULE
theModule = MODULE wikipediaModule

wikipediaModule :: WikipediaModule
wikipediaModule = WikipediaModule ()

instance Module WikipediaModule () where
    moduleName   _ = "wikipedia"
    moduleSticky _ = False

    moduleHelp _ s = return $ case s of
             "wikipedia" -> "search wikipedia and show url of first hit"
             _           -> "module for searching wikipedia"
    
    moduleCmds   _ = return ["wikipedia"]
    process _ _ src cmd rest = case cmd of
               "wikipedia" -> wikipediaCmd src rest
               _           -> error "wikipedia: invalid command"

------------------------------------------------------------------------

wikipediaCmd :: String -> String -> ModuleT s IRC ()
wikipediaCmd src rest = do 
        result <- liftIO $ query rest
        ircPrivmsg src (extractLoc $ tail result)

queryUrl :: String -> String
queryUrl q = "http://en.wikipedia.org/wiki/Special:Search?search="
             ++ urlEncode q

query :: String -> IO [String]
query q = readPage (proxy config) uri request ""
    where url = queryUrl q
          Just uri = parseURI url
          request = ["HEAD " ++ url ++ " HTTP/1.0", ""]

extractLoc :: [String] -> String
extractLoc headers = 
        fromMaybe (error "No matching header") 
                  (lookup "Location" $ concatMap f headers)

        where f s = case findIndex (==':') s of
                          Just n  -> [(take n s, drop (n+2) s)]
                          Nothing -> []
