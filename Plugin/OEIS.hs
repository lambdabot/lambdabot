--
-- | Look up sequences in the Online Encyclopedia of Integer Sequences
--   Based on the Math.OEIS library
--
module Plugin.OEIS (theModule) where

import Plugin

import Control.Arrow (second, (***))
import Math.OEIS
import Data.Char
import Network.URI

PLUGIN OEIS

instance Module OEISModule () where
    moduleCmds   _     = ["oeis"]
    moduleHelp _ _     = "oeis <sequence>. Look up a sequence in the Online Encyclopedia of Integer Sequences"
    process _ _ to _ a = do s <- liftIO $ lookupOEIS a
                            out <- mapM (ios80 to) (map return s)
                            return $ concat out


lookupOEIS :: String -> IO [String]
lookupOEIS a = do
         let a'  = commas . reverse . dropWhile isSpace . reverse . dropWhile isSpace $ a
         x <- searchSequence_IO a'
         case x of
            Nothing -> do r <- random insult
                          return ["Sequence not found. " ++ r]
            Just s  -> return [description s, show $ sequenceData s]
 where commas []                     = []
       commas (x:' ':xs) | isDigit x = x : ',' : commas xs
       commas (x:xs)                 = x : commas xs


-- | Look up a sequence in the OEIS using its search function
searchSequence_IO :: String -> IO (Maybe OEISSequence)
searchSequence_IO x = getOEIS (baseSearchURI++) (escapeURIString isAllowedInURI $ x)


baseSearchURI :: String
baseSearchURI = "http://www.research.att.com/~njas/sequences/?n=1&fmt=3&q="


getOEIS :: (a -> String) -> a -> IO (Maybe OEISSequence)
getOEIS toURI key = case parseURI (toURI key) of
                      Nothing  -> return Nothing
                      Just uri -> do content <- get uri
                                     case content of
                                       (Left LookupError) -> return Nothing
                                       (Right text) -> return $ parseOEIS text

data LookupError = LookupError deriving Show


parseOEIS :: String -> Maybe OEISSequence
parseOEIS x = if "no match" `isPrefixOf` (ls!!1)
                then Nothing
                else Just . foldl' (flip addElement) emptyOEIS . reverse . parseRawOEIS $ ls'
    where ls = lines x
          ls' = init . dropWhile ((/= "%") . take 1) $ ls

parseRawOEIS :: [String] -> [(Char, String)]
parseRawOEIS = map parseItem . combineConts


addElement :: (Char, String) -> (OEISSequence -> OEISSequence)
addElement ('I', x) c = c { catalogNums = words x }
addElement (t, x)   c | t `elem` "STU" = c { sequenceData = nums ++ (sequenceData c) }
    where nums = map read $ csvItems x
addElement (t, x)   c | t `elem` "VWX" = c { signedData = nums ++ (signedData c) }
    where nums = map read $ csvItems x
addElement ('N', x) c = c { description = x                  }
addElement ('D', x) c = c { references  = x : (references c) }
addElement ('H', x) c = c { links       = x : (links c)      }
addElement ('F', x) c = c { formulas    = x : (formulas c)   }
addElement ('Y', x) c = c { xrefs       = x : (xrefs c)      }
addElement ('A', x) c = c { author      = x                  }
addElement ('O', x) c = c { offset      = read o
                          , firstGT1    = read f }
  where (o,f) = second tail . span (/=',') $ x
addElement ('p', x) c = c { programs    = (Mathematica, x) :
                                            (programs c)     }
addElement ('t', x) c = c { programs    = (Maple, x) :
                                            (programs c)     }
addElement ('o', x) c = c { programs    = (Other, x) :
                                            (programs c)     }
addElement ('E', x) c = c { extensions  = x : (extensions c) }
addElement ('e', x) c = c { examples    = x : (examples c)   }
addElement ('K', x) c = c { keywords    = parseKeywords x    }
addElement ('C', x) c = c { comments    = x : (comments c)   }


emptyOEIS :: OEISSequence
emptyOEIS = OEIS [] [] [] "" [] [] [] [] "" 0 0 [] [] [] [] []


parseItem :: String -> (Char, String)
parseItem s = (c, str)
    where ( '%':c:_ , rest) = splitWord s
          ( _idNum, str )   = if (c == 'I') then ("", rest)
                                            else splitWord rest

combineConts :: [String] -> [String]
combineConts [] = []
combineConts [x] = [x]
combineConts (s@('%':_:_) : ss) =
  uncurry (:) . (joinConts s *** combineConts) . break isItem $ ss


joinConts :: String -> [String] -> String
joinConts s conts = s ++ (concat . map trimLeft $ conts)


csvItems :: String -> [String]
csvItems "" = []
csvItems x = item : others
    where (item, rest) = span (/=',') x
          others = csvItems $ del ',' rest


get :: URI -> IO (Either LookupError String)
get uri = do
    eresp <- getHtmlPage uri (proxy config)
    case eresp of
      [] -> return (Left LookupError)
      xs -> return (Right $ unlines $ dropWhile (/="\r") xs)


parseKeywords :: String -> [Keyword]
parseKeywords = map readKeyword . csvItems


splitWord :: String -> (String, String)
splitWord = second trimLeft . break isSpace


isItem :: String -> Bool
isItem x = not (null x) && '%' == head x


trimLeft :: String -> String
trimLeft = dropWhile isSpace


del :: Char -> String -> String
del _ ""     = ""
del c (x:xs) | c==x      = xs
             | otherwise = (x:xs)


readKeyword :: String -> Keyword
readKeyword = read . capitalize

capitalize :: String -> String
capitalize ""     = ""
capitalize (c:cs) = toUpper c : map toLower cs
