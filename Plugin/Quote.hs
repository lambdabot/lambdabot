{-# LANGUAGE TemplateHaskell, CPP, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
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

instance Module QuoteModule Quotes where
    moduleCmds           _ = ["quote", "remember", "forget", "ghc", "fortune"
                             ,"yow","arr","yarr","keal","b52s","brain","palomer"
                             ,"girl19", "v", "yhjulwwiefzojcbxybbruweejw"
                             , "protontorpedo", "nixon", "farber"]

    moduleHelp _ "forget"  = "forget nick quote.  Delete a quote"
    moduleHelp _ "fortune" = "fortune. Provide a random fortune"
    moduleHelp _ "yow"     = "yow. The zippy man."
    moduleHelp _ "arr"     = "arr. Talk to a pirate"
    moduleHelp _ "yarr"    = "yarr. Talk to a pirate"
    moduleHelp _ "keal"    = "keal. Talk like Keal"
    moduleHelp _ "ghc"     = "ghc. Choice quotes from GHC."
    moduleHelp _ "b52s"    = "b52s. Anyone noticed the b52s sound a lot like zippy?"
    moduleHelp _ "brain"   = "brain. Pinky and the Brain"
    moduleHelp _ "palomer" = "palomer. Sound a bit like palomer on a good day."
    moduleHelp _ "protontorpedo" = "protontorpedo is silly"
    moduleHelp _ "girl19"  = "girl19 wonders what \"discriminating hackers\" are."
    moduleHelp _ "v"       = "let v = show v in v"
    moduleHelp _ "yhjulwwiefzojcbxybbruweejw"
                           = "V RETURNS!"
    moduleHelp _ "farber"  = "Farberisms in the style of David Farber."
    moduleHelp _ "nixon"   = "Richad Nixon's finest."

    moduleHelp _ _         = help -- required

    moduleSerialize _       = Just mapListPackedSerial
    moduleDefState  _       = return M.empty

    process_ _ cmd s = case cmd of
          "forget"        -> runForget   (dropSpace s)
          "remember"      -> runRemember (dropSpace s)
          "quote"         -> runQuote    (dropSpace s)
          "ghc"           -> runQuote    ("ghc " ++ dropSpace s)
          "fortune"       -> runit (randFortune Nothing)
          "yow"           -> runit (randFortune (Just "zippy"))

          "keal"          -> rand kealList
          "b52s"          -> rand b52s
          "brain"         -> rand (if "pondering" `isInfixOf` s then brainPondering else brain)
          "palomer"       -> rand palomer
          "girl19"        -> rand girl19
          "protontorpedo" -> rand protontorpedo
          "v"             -> rand notoriousV
          "yhjulwwiefzojcbxybbruweejw"
                          -> rand notoriousV

          -- See, you've got to understand the subtle distinction in pirate
          -- talk between arr and yarr! arr is something you say as an
          -- afermative where as yarr! is more like a greeting. (Or something)
          "arr"           -> rand arrList
          "yarr"          -> rand yarrList
          "farber"        -> rand farberList
          "nixon"         -> rand nixonList

        where
           runit k = return `fmap` io k
           rand = runit . randomElem

help :: String
help = "quote <nick>\nremember <nick> <quote>\n" ++
       "Quote somebody, a random person, or save a memorable quote"

------------------------------------------------------------------------

-- the @remember command stores away a quotation by a user, for future
-- use by @quote

-- error handling!
runRemember :: String -> ModuleLB Quotes
runRemember str
    | null rest = return ["Incorrect arguments to quote"]
    | otherwise = withMS $ \fm writer -> do
        let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
            fm' = M.insert (P.pack nm) (P.pack q : ss) fm
        writer fm'
        r <- random confirmation
        box r
    where
        (nm,rest) = break isSpace str
        q         = tail rest

-- @forget, to remove a quote
runForget :: String -> ModuleLB Quotes
runForget str
    | null rest = return ["Incorrect arguments to quote"]
    | otherwise = withMS $ \fm writer -> do
        let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
            fm' = M.insert (P.pack nm) (delete (P.pack q) ss) fm
        writer fm'
        if P.pack q `elem` ss
            then return ["Done."]
            else return ["No match."]
    where
        (nm,rest) = break isSpace str
        q         = tail rest

--
--  the @quote command, takes a user nm to choose a random quote from
--
runQuote :: String -> ModuleLB Quotes
runQuote str = do
    st <- readMS
    io (search (P.pack nm) (P.pack pat) st)
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
