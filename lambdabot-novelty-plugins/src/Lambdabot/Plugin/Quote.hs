{-# LANGUAGE CPP, PatternGuards #-}
-- | Support for quotes
module Lambdabot.Plugin.Quote (theModule) where

import Lambdabot.Plugin

import qualified Data.ByteString.Char8 as P
import Data.Char
import Data.Fortune
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Text.Regex.TDFA

type Key    = P.ByteString
type Quotes = M.Map Key [P.ByteString]
type Quote  = ModuleT Quotes LB

theModule :: Module (M.Map P.ByteString [P.ByteString])
theModule = newModule
    { moduleSerialize = Just mapListPackedSerial
    , moduleDefState  = return M.empty
    , moduleInit      = modifyMS (M.filter (not . null))

    , moduleCmds = return
        [ (command "quote")
            { help = say "quote <nick>: Quote <nick> or a random person if no nick is given"
            , process = runQuote . dropSpace
            }
        , (command "remember")
            { help = say "remember <nick> <quote>: Remember that <nick> said <quote>."
            , process = runRemember . dropSpace
            }
        , (command "forget")
            { help = say "forget nick quote.  Delete a quote"
            , process = runForget . dropSpace
            }
        , (command "ghc")
            { help = say "ghc. Choice quotes from GHC."
            , process = const (fortune ["ghc"])
            }
        , (command "fortune")
            { help = say "fortune. Provide a random fortune"
            , process = const (fortune [])
            }
        , (command "yow")
            { help = say "yow. The zippy man."
            , process = const (fortune ["zippy"])
            }
        , (command "arr")
            { help = say "arr. Talk to a pirate"
            , process = const (fortune ["arr"])
            }
        , (command "yarr")
            { help = say "yarr. Talk to a scurvy pirate"
            , process = const (fortune ["arr", "yarr"])
            }
        , (command "keal")
            { help = say "keal. Talk like Keal"
            , process = const (fortune ["keal"])
            }
        , (command "b52s")
            { help = say "b52s. Anyone noticed the b52s sound a lot like zippy?"
            , process = const (fortune ["b52s"])
            }
        , (command "pinky")
            { help = say "pinky. Pinky and the Brain"
            , process = \s -> fortune $ if "pondering" `isInfixOf` s
                then ["pinky-pondering"]
                else ["pinky-pondering", "pinky"]
            }
        , (command "brain")
            { help = say "brain. Pinky and the Brain"
            , process = const (fortune ["brain"])
            }
        , (command "palomer")
            { help = say "palomer. Sound a bit like palomer on a good day."
            , process = const (fortune ["palomer"])
            }
        , (command "girl19")
            { help = say "girl19 wonders what \"discriminating hackers\" are."
            , process = const (fortune ["girl19"])
            }
        , (command "v")
            { aliases = ["yhjulwwiefzojcbxybbruweejw"]
            , help = getCmdName >>= \v -> case v of
                "v" -> say "let v = show v in v"
                _   -> say "V RETURNS!"
            , process = const (fortune ["notoriousV"])
            }
        , (command "protontorpedo")
            { help = say "protontorpedo is silly"
            , process = const (fortune ["protontorpedo"])
            }
        , (command "nixon")
            { help = say "Richard Nixon's finest."
            , process = const (fortune ["nixon"])
            }
        , (command "farber")
            { help = say "Farberisms in the style of David Farber."
            , process = const (fortune ["farber"])
            }
        ]
    }

fortune :: [FilePath] -> Cmd Quote ()
fortune xs = io (resolveFortuneFiles All xs >>= randomFortune) >>= say

------------------------------------------------------------------------

-- the @remember command stores away a quotation by a user, for future
-- use by @quote

-- error handling!
runRemember :: String -> Cmd Quote ()
runRemember str
    | null rest = say "Incorrect arguments to quote"
    | otherwise = do
        withMS $ \fm writer -> do
            let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
                fm' = M.insert (P.pack nm) (P.pack q : ss) fm
            writer fm'
        say =<< random confirmation
    where
        (nm,rest) = break isSpace str
        q         = drop 1 rest

-- @forget, to remove a quote
runForget :: String -> Cmd Quote ()
runForget str
    | null rest = say "Incorrect arguments to quote"
    | otherwise = do
        ss <- withMS $ \fm writer -> do
            let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
                fm' = case delete (P.pack q) ss of
                    []  -> M.delete (P.pack nm)     fm
                    ss' -> M.insert (P.pack nm) ss' fm
            writer fm'
            return ss
        say $ if P.pack q `elem` ss
            then "Done."
            else "No match."
    where
        (nm,rest) = break isSpace str
        q         = drop 1 rest

--
--  the @quote command, takes a user nm to choose a random quote from
--
runQuote :: String -> Cmd Quote ()
runQuote str =
    say =<< io . search (P.pack nm) (P.pack pat) =<< readMS
  where (nm, p) = break isSpace str
        pat     = drop 1 p

search :: Key -> P.ByteString -> Quotes -> IO String
search key pat db
    | M.null db          = return "No quotes yet."

    | P.null key         = do
        (key', qs) <- random (M.toList db) -- quote a random person
        fmap (display key') (random qs)

    | P.null pat, Just qs <- mquotes =
        fmap (display key) (random qs)

    | P.null pat         = match' key allquotes

    | Just qs <- mquotes = match' pat (zip (repeat key) qs)

    | otherwise          = do
        r <- random insult
        return $ "No quotes for this person. " ++ r

  where
    mquotes   = M.lookup key db
    allquotes = concat [ zip (repeat who) qs | (who, qs) <- M.assocs db ]

    match' p ss = do
        re <- makeRegexOptsM defaultCompOpt {caseSensitive = False, newSyntax = True}
                             defaultExecOpt {captureGroups = False} p

        let rs = filter (match re . snd) ss
        if null rs
            then do r <- random insult
                    return $ "No quotes match. " ++ r
            else do (who, saying) <- random rs
                    return $ P.unpack who ++ " says: " ++ P.unpack saying

    display k msg = (if P.null k then "  " else who ++ " says: ") ++ saying
          where saying = P.unpack msg
                who    = P.unpack k
