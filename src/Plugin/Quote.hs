{-# LANGUAGE TemplateHaskell, CPP, TypeFamilies, PatternGuards #-}
-- | Support for quotes
module Plugin.Quote (theModule) where

import Plugin
import Plugin.Quote.Fortune      (randFortune)
import Plugin.Quote.Text

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P

$(plugin "Quote")

type Key    = P.ByteString
type Quotes = M.Map Key [P.ByteString]

instance Module QuoteModule where
    type ModuleState QuoteModule = Quotes
    
    moduleCmds _ =
        [ (command "quote")
            { help = say genericHelp
            , process = runQuote . dropSpace
            }
        , (command "remember")
            { help = say genericHelp
            , process = runRemember . dropSpace
            }
        , (command "forget")
            { help = say "forget nick quote.  Delete a quote"
            , process = runForget . dropSpace
            }
        , (command "ghc")
            { help = say "ghc. Choice quotes from GHC."
            , process = runQuote . ("ghc " ++) . dropSpace
            }
        , (command "fortune")
            { help = say "fortune. Provide a random fortune"
            , process = const (runit (randFortune Nothing))
            }
        , (command "yow")
            { help = say "yow. The zippy man."
            , process = const (runit (randFortune (Just "zippy")))
            }
        , (command "arr")
            { help = say "arr. Talk to a pirate"
            , process = const (rand arrList)
            }
        , (command "yarr")
            { help = say "yarr. Talk to a pirate"
            , process = const (rand yarrList)
            }
        , (command "keal")
            { help = say "keal. Talk like Keal"
            , process = const (rand kealList)
            }
        , (command "b52s")
            { help = say "b52s. Anyone noticed the b52s sound a lot like zippy?"
            , process = const (rand b52s)
            }
        , (command "brain")
            { help = say "brain. Pinky and the Brain"
            , process = \s -> rand (if "pondering" `isInfixOf` s then brainPondering else brain)
            }
        , (command "palomer")
            { help = say "palomer. Sound a bit like palomer on a good day."
            , process = const (rand palomer)
            }
        , (command "girl19")
            { help = say "girl19 wonders what \"discriminating hackers\" are."
            , process = const (rand girl19)
            }
        , (command "v")
            { aliases = ["yhjulwwiefzojcbxybbruweejw"]
            , help = getCmdName >>= \v -> case v of
                "v" -> say "let v = show v in v"
                _   -> say "V RETURNS!"
            , process = const (rand notoriousV)
            }
        , (command "protontorpedo")
            { help = say "protontorpedo is silly"
            , process = const (rand protontorpedo)
            }
        , (command "nixon")
            { help = say "Richad Nixon's finest."
            , process = const (rand nixonList)
            }
        , (command "farber")
            { help = say "Farberisms in the style of David Farber."
            , process = const (rand farberList)
            }
        ]

    moduleSerialize _       = Just mapListPackedSerial
    moduleDefState  _       = return M.empty

runit :: IO String -> Cmd Quote ()
runit k = io k >>= say

rand :: [String] -> Cmd Quote ()
rand = runit . randomElem

genericHelp :: String
genericHelp = "quote <nick>\nremember <nick> <quote>\n" ++
       "Quote somebody, a random person, or save a memorable quote"

------------------------------------------------------------------------

-- the @remember command stores away a quotation by a user, for future
-- use by @quote

-- error handling!
runRemember :: String -> Cmd Quote ()
runRemember str
    | null rest = say "Incorrect arguments to quote"
    | otherwise = mapM_ say =<< lift (withMS $ \fm writer -> do
        let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
            fm' = M.insert (P.pack nm) (P.pack q : ss) fm
        writer fm'
        r <- random confirmation
        box r)
    where
        (nm,rest) = break isSpace str
        q         = tail rest

-- @forget, to remove a quote
runForget :: String -> Cmd Quote ()
runForget str
    | null rest = say "Incorrect arguments to quote"
    | otherwise = mapM_ say =<< lift (withMS $ \fm writer -> do
        let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
            fm' = M.insert (P.pack nm) (delete (P.pack q) ss) fm
        writer fm'
        if P.pack q `elem` ss
            then return ["Done."]
            else return ["No match."])
    where
        (nm,rest) = break isSpace str
        q         = tail rest

--
--  the @quote command, takes a user nm to choose a random quote from
--
runQuote :: String -> Cmd Quote ()
runQuote str = mapM_ say =<< lift (do
    st <- readMS
    io (search (P.pack nm) (P.pack pat) st))
  where (nm, p) = break isSpace str
        pat     = if null p then p else tail p

search :: Key -> P.ByteString -> Quotes -> IO [String]
search key pat db
    | M.null db          = box "No quotes yet."

    | P.null key         = do
        (key', qs) <- random (M.toList db) -- quote a random person
        box . display key' =<< random qs

    | P.null pat, Just qs <- mquotes =
        box . display key  =<< random qs

    | P.null pat         = match key allquotes

    | Just qs <- mquotes = match pat (zip (repeat key) qs)

    | otherwise          = do
        r <- random insult
        box $ "No quotes for this person. " ++ r

  where
    mquotes   = M.lookup key db
    allquotes = concat [ zip (repeat who) qs | (who, qs) <- M.assocs db ]

    match p ss = do
#if __GLASGOW_HASKELL__ >= 606
        re <- do res <- compile (compExtended + compIgnoreCase + compNoSub) 0 p
                 case res of
                    Left  err -> error $ "regex failed: " ++ show err
                    Right r   -> return r
#else
        let re = mkRegexWithOpts (P.unpack p) True True
#endif

        let rs = filter (matches re . snd) ss
        if null rs
            then do r <- random insult
                    box $ "No quotes match. " ++ r
            else do (who, saying) <- random rs
                    box $ P.unpack who ++ " says: " ++ P.unpack saying

    display k msg = (if P.null k then "  " else who ++ " says: ") ++ saying
          where saying = P.unpack msg
                who    = P.unpack k
