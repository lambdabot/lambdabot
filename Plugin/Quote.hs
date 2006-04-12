--
-- | Support for quotes
--
module Plugin.Quote (theModule) where

import Plugin.Quote.Fortune      (randFortune)
import Plugin.Quote.Text

import Plugin

import qualified Data.Map as M
import qualified Data.FastPackedString as P

PLUGIN Quote

type Quotes = M.Map P.FastString [P.FastString]

instance Module QuoteModule Quotes where
    moduleCmds           _ = ["quote", "remember", "ghc", "fortune"
                             ,"yow","arr","keal","b52s","brain","palomer","girl19"]

    moduleHelp _ "fortune" = "fortune. Provide a random fortune"
    moduleHelp _ "yow"     = "yow. The zippy man."
    moduleHelp _ "arr"     = "arr. Talk to a pirate"
    moduleHelp _ "keal"    = "keal. Talk like Keal"
    moduleHelp _ "ghc"     = "ghc. Choice quotes from GHC."
    moduleHelp _ "b52s"    = "b52s. Anyone noticed the b52s sound a lot like zippy?"
    moduleHelp _ "brain"   = "brain. Pinky and the Brain"
    moduleHelp _ "palomer" = "palomer. Sound a bit like palomer on a good day."
    moduleHelp _ "girl19"  = "girl19 wonders what \"discriminating hackers\" are."
    moduleHelp _ _         = help -- required

    moduleSerialize _       = Just mapListPackedSerial
    moduleDefState  _       = return M.empty

    process_ _ cmd s = case cmd of
          "remember" -> runRemember (dropSpace s)
          "quote"    -> runQuote    (dropSpace s)
          "ghc"      -> runQuote    "ghc"
          "fortune"  -> return `fmap` io (randFortune Nothing)
          "yow"      -> return `fmap` io (randFortune (Just "zippy"))
          "keal"     -> return `fmap` io (randomElem kealList)
          "arr"      -> return `fmap` io (randomElem arrList)
          "b52s"     -> return `fmap` io (randomElem b52s)
          "brain"    -> return `fmap` io (randomElem brain)
          "palomer"  -> return `fmap` io (randomElem palomer)
          "girl19"   -> return `fmap` io (randomElem girl19)

help :: String
help = "quote <nick>\nremember <nick> <quote>\n\ 
       \Quote somebody, a random person, or save a memorable quote"

------------------------------------------------------------------------

-- the @remember command stores away a quotation by a user, for future
-- use by @quote

-- error handling!
runRemember :: String -> ModuleLB Quotes
runRemember str = do
    case break (== ' ') str of
        (_,[])    -> return ["Incorrect arguments to quote"]
        (nm,q') -> do let q = tail q'
                      withMS $ \fm writer -> do
                        let ss  = fromMaybe [] (M.lookup (P.pack nm) fm)
                            fm' = M.insert (P.pack nm) (P.pack q : ss) fm
                        writer fm'

                        -- and flush to disk too, just in case.
                        -- should probably have general support for this.
                        -- maybe even a @flush plugin.
                        io $ P.writeFile ("State/quote") (showPacked fm')

                        return ["Done."]

--
--  the @quote command, takes a user nm to choose a random quote from
--
runQuote :: String -> ModuleLB Quotes
runQuote name' = do
    fm <- readMS
    if M.null fm then return ["No quotes yet."] else do

        let pnm = P.pack name'
            qs' = M.lookup pnm fm

        (nm,qs) <- if not (P.null pnm)
                   then return (pnm,qs') -- (FastString, Maybe [FastString])
                   else do (nm',rs') <- io $ randomElem (M.toList fm) -- random person
                           return (nm', Just rs')
        case qs of
            Nothing   -> return [P.unpack nm ++ " hasn't said anything memorable"]
            Just msgs -> do msg <- io $ randomElem msgs
                            return $ if not (P.null pnm)
                                then ["  " ++ (P.unpack msg)]
                                else [(P.unpack nm)++" says: " ++ (P.unpack msg)]


