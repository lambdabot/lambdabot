--
-- | DICT (RFC 2229) Lookup 
-- Tom Moertel <tom@moertel.com>
-- 
--Here's how you might write a program to query the Jargon database for
--the definition of "hacker" and then print the result:
--
-- >  main = doJargonLookup "hacker" >>= putStr
-- >
-- >  doJargonLookup :: String -> IO String
-- >  doJargonLookup query = do
-- >      result <- simpleDictLookup (QC "dict.org" 2628) "jargon" query 
-- >      return $ case result of
-- >          Left errorResult -> "ERROR: " ++ errorResult
-- >          Right dictResult -> dictResult
-- >
--
module Plugin.Dict.DictLookup ( simpleDictLookup, QueryConfig(..), LookupResult) where

import Data.List
import System.IO
import Control.OldException (handle)
import Network

data QueryConfig    = QC { host :: String, port :: Int }
type DictConnection = Handle
data DictCommand    = Quit | Define DictName String
type DictName       = String -- dict-db name | "!" 1st match | "*" all matches
type LookupResult   = Either String String -- Left <error> | Right <result>

simpleDictLookup :: QueryConfig -> DictName -> String -> IO LookupResult
simpleDictLookup config dictnm query =
    handle (\e -> (return $ Left (show e))) $ do
        conn <- openDictConnection config
        result <- queryDict conn dictnm query
        closeDictConnection conn
        return result

openDictConnection :: QueryConfig -> IO DictConnection
openDictConnection config = do
    hDictServer <- connectTo (host config) (mkPortNumber $ port config)
    hSetBuffering hDictServer LineBuffering
    readResponseLine hDictServer -- ignore response
    return hDictServer
    where
    mkPortNumber = PortNumber . fromIntegral

closeDictConnection :: DictConnection -> IO ()
closeDictConnection conn = do
    sendCommand conn Quit
    readResponseLine conn -- ignore response
    hClose conn

{-
queryAllDicts :: DictConnection -> String -> IO LookupResult
queryAllDicts = flip queryDict "*"
-}

queryDict :: DictConnection -> DictName -> String -> IO LookupResult
queryDict conn dictnm query = do
    sendCommand conn (Define dictnm query)
    response <- readResponseLine conn
    case response of
        '1':'5':_     -> readDefinition >>= return . formatDefinition
        '5':'5':'2':_ -> return $ Right ("No match for \"" ++ query ++ "\".\n")
        '5':_         -> return $ Left response -- error response
        _             -> return $ Left ("Bogus response: " ++ response)
            
    where

    readDefinition = do
        line <- readResponseLine conn
        case line of
            '2':'5':'0':_ -> return []
            _             -> readDefinition >>= return . (line:)

    formatDefinition = Right . unlines . concatMap formater

    formater ('1':'5':'1':rest) = ["", "***" ++ rest]
    formater "."                = []
    formater line               = [line]


readResponseLine :: DictConnection -> IO String
readResponseLine conn = do
    line <- hGetLine conn
    return (filter (/='\r') line)

sendCommand :: DictConnection -> DictCommand -> IO ()
sendCommand conn cmd =
    hSendLine conn $ case cmd of
        Quit -> "QUIT"
        Define db target -> join " " ["DEFINE", db, target]

join :: [a] -> [[a]] -> [a]
join = (concat.) . intersperse

hSendLine :: Handle -> String -> IO ()
hSendLine h line = hPutStr h (line ++ "\r\n")
