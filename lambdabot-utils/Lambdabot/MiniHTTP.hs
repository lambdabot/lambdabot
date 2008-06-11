-- | HTTP protocol binding.
-- <http://homepages.paradise.net.nz/warrickg/haskell/http/>
-- <http://www.dtek.chalmers.se/~d00bring/haskell-xml-rpc/http.html>

module Lambdabot.MiniHTTP (
        Proxy,
        mkPost,
        readPage,
        readNBytes,
        urlEncode,
        urlDecode,
        module Network.URI
    ) where

import Control.Monad (liftM2)
import Data.Bits  ((.&.))
import Data.Char  (ord, chr, digitToInt, intToDigit)
import Data.Maybe (fromMaybe)
import Network
import Network.URI hiding (authority)
import System.IO

authority :: URI -> String
authority = uriRegName . maybe (error "authority") id . uriAuthority

type Proxy = Maybe (String, Integer)

-- HTTP specific stuff
mkPost :: URI -> String -> [String]
mkPost uri body = ["POST " ++ url ++ " HTTP/1.0",
                   "Host: " ++ host,
                   "Accept: */*",
                   "Content-Type: application/x-www-form-urlencoded",
                   "Content-Length: " ++ (show $ length body),
                   ""]
    where
    url = show uri
    host = authority uri

hGetLines :: Handle -> IO [String]
hGetLines h = do
  eof <- hIsEOF h
  if eof then return []
     else
     liftM2 (:) (hGetLine h) (hGetLines h)

readPage :: Proxy -> URI -> [String] -> String -> IO [String]
readPage proxy uri headers body = withSocketsDo $ do
      h <- connectTo host (PortNumber (fromInteger port))
      mapM_ (\s -> hPutStr h (s ++ "\r\n")) headers
      hPutStr h body
      hFlush h
      contents <- hGetLines h
      hClose h
      return contents
    where
    (host, port) = fromMaybe (authority uri, 80) proxy

--
-- read lines, up to limit of n bytes. Useful to ensure people don't
-- abuse the url plugin
--
readNBytes :: Int -> Proxy -> URI -> [String] -> String -> IO [String]
readNBytes n proxy uri headers body = withSocketsDo $ do
      h <- connectTo host (PortNumber (fromInteger port))
      mapM_ (\s -> hPutStr h (s ++ "\r\n")) headers
      hPutStr h body
      hFlush h
      contents <- lines `fmap` hGetN n h
      hClose h
      return contents
    where
    (host, port) = fromMaybe (authority uri, 80) proxy

    hGetN :: Int -> Handle -> IO String
    hGetN i h | i `seq` h `seq` False = undefined -- strictify
    hGetN 0 _ = return []
    hGetN i h = do eof <- hIsEOF h
                   if eof then return []
                          else liftM2 (:) (hGetChar h) (hGetN (i-1) h)

-- from HTTP.hs
urlEncode, urlDecode :: String -> String

urlDecode ('%':a:b:rest) = chr (16 * digitToInt a + digitToInt b)
                         : urlDecode rest
urlDecode (h:t) = h : urlDecode t
urlDecode [] = []

urlEncode (h:t) =
    let str = if isReservedChar(ord h) then escape h else [h]
    in str ++ urlEncode t
  where
        isReservedChar x
            | x >= ord 'a' && x <= ord 'z' = False
            | x >= ord 'A' && x <= ord 'Z' = False
            | x >= ord '0' && x <= ord '9' = False
            | x <= 0x20 || x >= 0x7F = True
            | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                           ,'=','+',',','$','{','}'
                                           ,'|','\\','^','[',']','`'
                                           ,'<','>','#','%','"']
        -- wouldn't it be nice if the compiler
        -- optimised the above for us?

        escape x =
            let y = ord x
            in [ '%', intToDigit ((y `div` 16) .&. 0xf), intToDigit (y .&. 0xf) ]

urlEncode [] = []
