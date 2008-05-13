{-# LANGUAGE MultiParamTypeClasses #-}

-- | Pull quotes down from yahoo.
module Plugin.Ticker (theModule) where

import Plugin
import Text.Printf

PLUGIN Ticker

instance Module TickerModule () where
    moduleHelp _ s      = case s of
         "ticker"     -> "quote ticker.  Look up quotes for ticker"
    moduleCmds      _   = ["ticker"]
    process_ _ s       e = lift $ quoteCmd s e

------------------------------------------------------------------------

quoteCmd :: String -> String -> LB [String]
quoteCmd _ []        = return ["Empty quote."]
quoteCmd _ ticker = do
    body    <- io $ queryit "GET" (quoteUrl ticker)
    case extractQuote body of
      Just r -> return $ [r]
      Nothing  -> return ["No Result Found."]

-- fetch: s symbol, l1 price, c change with percent, d1 date, t1 time.
quoteUrl :: String -> String
quoteUrl ticker =  "http://download.finance.yahoo.com/d/quotes.csv?f=sl1cd1t1&e=.csv&s=" ++ urlEncode ticker

queryit :: String -> String -> IO [String]
queryit meth url = readPage (proxy config) uri request ""
    where Just uri = parseURI url
          abs_path = uriPath uri ++ uriQuery uri ++ uriFragment uri
          request  = case proxy config of
                        Nothing -> [meth ++ " " ++ abs_path ++ " HTTP/1.0", ""]
                        _       -> [meth ++ " " ++ url ++ " HTTP/1.0", ""]

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile p xs = (takeWhile p xs, dropWhile p xs)

-- | split string on commas.
-- Quotes allowed in CSV if it's the first character of a field.
csv :: String -> [String]
csv ('"':xs) = case splitWhile (/= '"') xs of
                  (word, '"':',':rest) -> word : csv rest
                  (word, '"':[])       -> word : []
                  _                    -> error "invalid CSV"
csv xs = case splitWhile (/= ',') xs of
             (word, ',':rest) -> word : csv rest
             ([], [])         -> []
             (word, [])       -> [word]
             _                -> error "shouldn't happen"

-- $ curl "http://download.finance.yahoo.com/d/quotes.csv?f=sl1cd1t1&e=.csv&s=C"
-- "C",23.19,"-0.45 - -1.90%","5/13/2008","1:32pm"
-- "GBPUSD=X",1.9478,"N/A - N/A","5/13/2008","1:52pm"
extractQuote :: [String] -> Maybe String
extractQuote [] = Nothing
extractQuote ls = (getQuote . csv . filter (/= '\r') . last) ls
    where
        getQuote (_:ticker:price:date:time:change:afterhrs:_) = 
            Just $ printf "%s: %s %s %s @ %s %s %s" ticker' price change perc date' time' afterhrs'
            where ticker' = unquote ticker
                  time' = unquote time
                  date' = unquote date
                  price' = val price
                  change' = val change
		  afterhrs' = if (val afterhrs) > 0.00
                                 then printf "(AH %s)" afterhrs
                                 else ""
                  perc = if change == "N/A"
                            then "-"
                            else printf "(%.1f%%)" (100.0 * change' / (price' - change'))
        getQuote _ = Nothing

