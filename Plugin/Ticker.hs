{-# LANGUAGE MultiParamTypeClasses #-}

-- | Pull quotes down from yahoo.
module Plugin.Ticker (theModule) where

import Plugin
import Control.Arrow
import Text.Printf

PLUGIN Ticker

instance Module TickerModule () where
    moduleHelp _ s      = case s of
         "ticker"     -> "quote ticker.  Look up quotes for ticker"
    moduleCmds      _   = ["ticker", "condor"]
    process_ _ "ticker" e = lift $ tickerCmd e
    process_ _ "condor" e = lift $ condorCmd e

------------------------------------------------------------------------

tickerCmd :: String -> LB [String]
tickerCmd []        = return ["Empty ticker."]
tickerCmd tickers = do
    body    <- io $ fetchPage "GET" (tickerUrl tickers)
    case [x | Just x <- map extractQuote body] of
      []       -> return ["No Result Found."]
      xs       -> return xs

-- fetch: s symbol, l1 price, c change with percent, d1 date, t1 time.
tickerUrl :: String -> String
tickerUrl tickers =  "http://download.finance.yahoo.com/d/quotes.csv?f=sl1cd1t1&e=.csv&s=" ++ ts
    where ts = concatWith "+" $ map urlEncode $ words tickers

-- $ curl "http://download.finance.yahoo.com/d/quotes.csv?f=sl1cd1t1&e=.csv&s=C"
-- "C",23.19,"-0.45 - -1.90%","5/13/2008","1:32pm"
-- "GBPUSD=X",1.9478,"N/A - N/A","5/13/2008","1:52pm"
extractQuote :: String -> Maybe String
extractQuote s = (getQuote . csv) s
    where
        getQuote [sym, price, change, date, time] =
            Just $ printf "%s: %s %s@ %s %s" sym price change' date time
            where change' = case words change of
                              ("N/A":_)    -> ""
                              [ch, _, pch] -> ch ++ " (" ++ pch ++ ") "
                              _            -> ""
        getQuote _ = Nothing

condorCmd :: String -> LB [String]
condorCmd [] = return ["Empty ticker list"]
condorCmd tickers = liftM (:[]) $ io (calcCondor tickers)

-- fetch: b bid, a ask
condorUrl :: String -> String
condorUrl tickers =  "http://download.finance.yahoo.com/d/quotes.csv?f=ba&e=.csv&s=" ++ ts
    where ts = concatWith "+" $ map urlEncode $ words tickers

getBidAsk :: String -> IO (Maybe (Float, Float))
getBidAsk ticker = do
    xs <- fetchPage "GET" (condorUrl ticker)
    case xs of
        (x:_) -> (return . extractPrice . csv) x
        _     -> return Nothing
    where
        extractPrice [bid,ask] = Just (read bid, read ask)
        extractPrice _ = Nothing

calcCondor :: String -> IO String
calcCondor s = do
    let ws = splitList '/' s
    xs <- mapM getBidAsk ws
    return $ case xs of
        [Just (ab,aa), Just (bb,ba), Just (cb,ca), Just (db,da)] -> 
              printf "%s: bid $%.02f  ask $%.02f" s (bb+cb-aa-da) (ba+ca-ab-db)
        [Nothing, _, _, _] -> printf "Can't find '%s'" (ws !! 0)
        [_, Nothing, _, _] -> printf "Can't find '%s'" (ws !! 1)
        [_, _, Nothing, _] -> printf "Can't find '%s'" (ws !! 2)
        [_, _, _, Nothing] -> printf "Can't find '%s'" (ws !! 3)
        _ -> printf "Bad format '%s'" s

---- Library routines, consider moving elsewhere. ----

-- | Fetch a page via HTTP and return its body as a list of lines.
fetchPage :: String -> String -> IO [String]
fetchPage meth url = liftM cleanup $ readPage (proxy config) uri request ""
    where Just uri = parseURI url
          abs_path = uriPath uri ++ uriQuery uri ++ uriFragment uri
          request  = case proxy config of
                        Nothing -> [meth ++ " " ++ abs_path ++ " HTTP/1.0", ""]
                        _       -> [meth ++ " " ++ url ++ " HTTP/1.0", ""]
          dropHdr = drop 1 . dropWhile (not.null)
          cleanup = dropHdr . (map (filter (/= '\r')))

-- | Split a list into two lists based on a predicate.
splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile p xs = (takeWhile p xs, dropWhile p xs)

-- | Unfoldr, continuing while the predicate is true.
unfoldrp :: (b -> Bool) -> (b -> Maybe (a, b)) -> b -> [a]
unfoldrp p f = unfoldr (\x -> guard (p x) >> f x)

-- | Split a list on a distinguished character (which is eliminated).
splitList :: (Eq a) => a -> [a] -> [[a]]
splitList ch = unfoldrp (not.null) (return.f)
    where f = (second (drop 1)) . (splitWhile (/= ch))

-- | Return a list of comma-separated values.
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

