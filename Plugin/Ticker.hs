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

quoteUrl :: String -> String
quoteUrl ticker =  "http://download.finance.yahoo.com/d/quotes.csv?f=nsl1d1t1c1b2&e=.csv&s=" ++ urlEncode ticker

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
                  _                    -> error "invalid CSV"
csv xs = case splitWhile (/= ',') xs of
             (word, ',':rest) -> word : csv rest
             ([], [])         -> []
             (word, [])       -> [word]
             _                -> error "shouldn't happen"

-- example output:
-- "S&P DEP RECEIPTS","SPY",138.32,"4/24/2008","4:15pm",+0.60,138.32
extractQuote :: [String] -> Maybe String
extractQuote [] = Nothing
extractQuote ls = (getQuote . csv . last) ls
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
        unquote cs = filter (/= '"') cs
        val :: String -> Double
	val "N/A" = 0.0
        val ('+':xs) = read xs
        val xs = read xs

