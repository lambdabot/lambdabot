{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

-- | Pull quotes down from yahoo.
module Plugin.Ticker (theModule) where

import Plugin hiding((<$>))
import Data.Either
import Control.Applicative ((<$>))
-- import Control.Arrow (second)
import Text.Printf

$(plugin "Ticker")

instance Module TickerModule () where
    moduleCmds      _     = ["ticker", "bid"]
    moduleHelp _ s        = case s of
         "ticker" -> "ticker symbols.  Look up quotes for symbols"
         "bid"    -> "bid symbols.  Sum up the bid and ask prices for symbols."
    process_ _ "ticker" e = lift $ tickerCmd e
    process_ _ "bid"    e = lift $ bidsCmd e

------------------------------------------------------------------------

-- Fetch several ticker quotes and report them.
tickerCmd :: String -> LB [String]
tickerCmd []        = return ["Empty ticker."]
tickerCmd tickers = do
    quotes <- io $ fetchPage "GET" $ tickerUrl $ words tickers
    case [x | Just x <- map extractQuote quotes] of
      []       -> return ["No Result Found."]
      xs       -> return xs

-- fetch: s symbol, l1 price, c change with percent, d1 date, t1 time.
tickerUrl :: [String] -> String
tickerUrl tickers =  "http://download.finance.yahoo.com/d/quotes.csv?f=sl1cd1t1&e=.csv&s=" ++ ts
    where ts = concatWith "+" $ map urlEncode tickers

-- $ curl "http://download.finance.yahoo.com/d/quotes.csv?f=sl1cd1t1&e=.csv&s=C"
-- "C",23.19,"-0.45 - -1.90%","5/13/2008","1:32pm"
-- "GBPUSD=X",1.9478,"N/A - N/A","5/13/2008","1:52pm"
extractQuote :: String -> Maybe String
extractQuote = getQuote . csv
    where
        getQuote [sym, price, change, date, time] =
            Just $ printf "%s: %s %s@ %s %s" sym price change' date time
            where change' = case words change of
                              ("N/A":_)    -> ""
                              [ch, _, pch] -> ch ++ " (" ++ pch ++ ") "
                              _            -> ""
        getQuote _ = Nothing

-- Fetch quotes for tickers and sum their bid/ask prices.
bidsCmd :: String -> LB [String]
bidsCmd tickers =
    case words tickers of
        [] -> return [printf "Invalid argument '%s'" tickers]
        xs -> io $ (:[]) <$> calcBids xs

-- fetch: b bid, a ask
bidsUrl :: [String] -> String
bidsUrl tickers = "http://download.finance.yahoo.com/d/quotes.csv?f=ba&e=.csv&s=" ++ ts
    where ts = concatWith "+" $ map urlEncode tickers

getBidAsks :: [String] -> IO [Maybe (Float, Float)]
getBidAsks tickers = do
    xs <- fetchPage "GET" $ bidsUrl tickers
    return $ map (extractPrice.csv) xs
    where
        extractPrice :: [String] -> Maybe (Float, Float)
        extractPrice [bid,ask] = liftM2 (,) (readMaybe bid) (readMaybe ask)
        extractPrice _         = Nothing

type AccumVal = Either String (Float, Float)

-- If we have a new bid/ask pair, accumulate it (normally add, but
-- if the ticker starts with '-' then subtract).  If there is no
-- value, make a note that it is an error.
accumOption :: AccumVal -> (String, Maybe (Float, Float)) -> AccumVal
accumOption err@(Left _) _ = err
accumOption (Right _) (ticker, Nothing) = Left $ printf "Can't find '%s'" ticker
accumOption (Right (a,b)) (('-':_), Just (a',b')) = Right (a-b', b-a')
accumOption (Right (a,b)) (_, Just (a',b')) = Right (a+a', b+b')

-- Take a list of tickers which are optionally prefixed with '+' or '-'
-- and add up (or subtract) the bid/ask prices on the based on the prefix.
calcBids :: [String] -> IO String
calcBids ticks = do
    xs <- getBidAsks $ map noPrefix ticks
    return $ case foldl accumOption (Right (0,0)) (zip ticks xs) of
        (Left err)        -> err
        (Right (bid,ask)) -> printf "%s: bid $%.02f, ask $%.02f" s bid ask
    where
        s = unwords ticks
        noPrefix ('+':xs) = xs
        noPrefix ('-':xs) = xs
        noPrefix xs = xs


---- Library routines, consider moving elsewhere. ----

-- | Fetch a page via HTTP and return its body as a list of lines.
fetchPage :: String -> String -> IO [String]
fetchPage meth url = cleanup <$> readPage (proxy config) uri request ""
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
{-
unfoldrp :: (b -> Bool) -> (b -> Maybe (a, b)) -> b -> [a]
unfoldrp p f = unfoldr (\x -> guard (p x) >> f x)
-}

-- | Split a list on a distinguished character (which is eliminated).
{-
splitList :: (Eq a) => a -> [a] -> [[a]]
splitList ch = unfoldrp (not.null) (return.f)
    where f = (second (drop 1)) . (splitWhile (/= ch))
-}

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

-- | Read a value from a string.
readMaybe :: Read a => String -> Maybe a
readMaybe x = case readsPrec 0 x of
                [(y,"")] -> Just y
                _        -> Nothing

