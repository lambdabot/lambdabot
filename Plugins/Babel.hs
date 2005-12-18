--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2004 Simon Winwood - http://www.cse.unsw.edu.au/~sjw
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | A translator module for lambdabot, also provides some quote code.
--
module Plugins.Babel (theModule) where

import Lambdabot
import LBState
import Serial
import Util         hiding (split)
import MiniHTTP
import Config               (proxy,config)
import qualified Map as M
import qualified Data.FastPackedString as P

import Data.Maybe           (fromMaybe,fromJust)
import Data.Char            (toLower)
import Data.List

import Control.Monad.Error

import Network.URI
import Text.Regex

------------------------------------------------------------------------

newtype BabelModule = BabelModule ()

theModule :: MODULE
theModule = MODULE $ BabelModule ()

type Quotes = M.Map P.FastString [P.FastString]

instance Module BabelModule Quotes where
        moduleSerialize _       = Just mapListPackedSerial
        moduleDefState  _       = return M.empty

        moduleCmds _ = ["babel", "remember", "quote", "ghc"]

        moduleHelp _ "babel"    = concat ["usage: babel lang lang phrase"]
        moduleHelp _ "ghc"      = "GHC!"
        moduleHelp _ _          = help

        process_ m "remember"  s = process_ m "quote-add" (dropSpace s) -- synonym
        process_ _ "babel"     s = run_babel s
        process_ _ "quote-add" s = run_remember (dropSpace s)
        process_ _ "quote"     s = run_quote    (dropSpace s)
        process_ _ "ghc"       _ = run_quote    "ghc"

--      moduleHelp _ "timein"   = return "@timein <city>, report the local time in <city>"
--      process _ _ src "last"  s     = run_last  src s

        {-
        -- totally unrelated :}
        process _ _ src "timein" s =
          if s == "help"
            then ircPrivmsg src "  http://www.timeanddate.com"
            else do (o,_,_) <- liftIO $ popen "timein" [s] Nothing
                    ircPrivmsg src $ "  " ++ o
        -}

help :: String
help = "@quote <nick>/@quote-add <nick> <quote>\n" ++
   "Quote somebody, or a random person, or save a memorable quote"

--
-- The @babel command.
--
--      * translate a phrase
--      * translate last 'i' lines of history buffer
--      * translate a line of history indexed by a regex
--
-- TODO add range/context. i.e. !f-3 or 5-4
--
run_babel :: String -> LB [String]
run_babel s = do
        let cmd = split ' ' 3 s
        msg <- run_babel' cmd
        let msg' = map ("  " ++) msg
        return [unlines msg']

-- help msg
run_babel' :: (MonadIO m) => [String] -> m [String]
run_babel' ["help"]      = return $ ["usage: babel lang lang phrase"]
run_babel' ["languages"] = return $ [show shortLangs]
run_babel' [f,t,i]       = do p <- liftIO $ babelFish f t i ; return [p]
run_babel' _             = return ["bzzt."]

{-
--
-- grab the history and translate it
--
doHistory :: String -> String -> Int -> LB [String]
doHistory f t i = do
        ss <- getHistory i
        if (null ss)
            then return []
            else do let (ns,ms) = unzip ss
                    ms' <- liftIO $ mapM (babelFish f t) ms
                    return $! indent (ns,ms')

--
-- given a regex, find the most recent line in the history that
-- matches the regex, and translate it
--
-- have to reverse history, as we want to search from the most recent
-- backwards.
--
doLookup :: String -> String -> String -> LB [String]
doLookup f t r = do
        ss <- getHistory 100 -- all the lines
        case find matches (reverse ss) of
                Nothing    -> return []
                Just (n,v) -> do v' <- liftIO $ babelFish f t v
                                 return $! indent ([n],[v'])
    where regex = mkRegex r
          matches (_nic,s) = isJust $ regex `matchRegex` s

isLookup ('!':_) = True
isLookup _       = False

-- is this going to parse as an integer?
isNum :: [Char] -> Bool
isNum ss = foldr (&&) True (map isDigit ss)

-- pretty print the history, in  "nic> msg" form
indent :: ([[Char]], [[Char]]) -> [[Char]]
indent (ns,ms) = zipWith (\a b -> a++"> "++b) ns ms

-- ---------------------------------------------------------------------
-- the @last command
--
run_last :: String -> String -> LB ()

-- no args, return 5 lines
run_last src [] = do hs <- getHistory 5
                     let o = map (\s -> "  "++s) (indent (unzip hs))
                     ircPrivmsg src (unlines o)

-- otherwise, 'i' lines of history
run_last src i = do
        o <- if isNum i
             then do hs <- getHistory (read i)
                     return $! indent (unzip hs)
             else return ["bzzt."]
        let o' = map (\s -> "  "++s) o
        ircPrivmsg src (unlines o')
-}

------------------------------------------------------------------------

-- the @remember command stores away a quotation by a user, for future
-- use by @quote

run_remember :: String -> ModuleLB Quotes
run_remember str = do
    let (name,q') = break (== ' ') str
        q = if null q' then q' else tail q'
    withMS $ \fm writer -> do
      let ss  = fromMaybe [] (M.lookup (P.pack name) fm)
          fm' = M.insert (P.pack name) (P.pack q : ss) fm
      writer fm'
    return []

--
--  the @quote command, takes a user name to choose a random quote from
--
run_quote :: String -> ModuleLB Quotes
run_quote name = do
    fm <- readMS
    let pnm = P.pack name
        qs' = M.lookup pnm fm

    (nm,qs) <- if not (P.null pnm)
               then return (pnm,qs') -- (FastString, Maybe [FastString])
               else do (nm',rs') <- liftIO $ stdGetRandItem (M.toList fm) -- random person
                       return (nm', Just rs')
    case qs of
        Nothing   -> return [P.unpack nm ++ " hasn't said anything memorable"]
        Just msgs -> do msg <- liftIO $ stdGetRandItem msgs
                        return $ if not (P.null pnm)
                            then ["  " ++ (P.unpack msg)]
                            else [(P.unpack nm)++" says: " ++ (P.unpack msg)]

------------------------------------------------------------------------

babelFishURL :: [Char]
babelFishURL = "http://babelfish.altavista.com/babelfish/tr"

supportedLangs :: [([Char], [Char])]
supportedLangs =
    [("german", "de"),
     ("greek", "el"),
     ("english", "en"),
     ("spanish", "es"),
     ("french", "fr"),
     ("italian", "it"),
     ("japanese", "ja"),
     ("korean", "ko"),
     ("dutch", "nl"),
     ("portuguese", "pt"),
     ("russian", "ru"),
     ("chinese-simp", "zh"),
     ("chinese-trad", "zt")]

shortLangs :: [[Char]]
shortLangs = ["de","el","en","es","fr","it","ja","ko","nl","pt","ru","zh","zt"]

type URIVars = [(String, String)]

constantVars :: URIVars
constantVars =
    [("doit", "done"),
     ("intl", "1"),
     ("tt", "urltext")]

justEither :: a -> Maybe b -> Either a b
justEither a Nothing = Left a
justEither _ (Just v) = Right v

langVars :: String -> String -> URIVars -> Either String URIVars
langVars inLang outLang vars =
    lookupLang inLang >>= \ins ->
        lookupLang outLang >>= \outs ->
            Right (("lp", ins ++ "_" ++ outs) : vars)
    where
    lookupLang lang | length lang == 2 && lang `elem` shortLangs = Right lang
    lookupLang lang = justEither ("Language " ++ lang ++ " not supported")
                                 (lookup (map toLower lang) supportedLangs)

mkBody :: String -> String -> String -> Either String String
mkBody inLang outLang string
    = mapconcat mkString "&" vars
    where
    vars = langVars inLang outLang (("urltext", string) : constantVars)
    mkString (var, val) = var ++ "=" ++ (urlEncode val)
    mapconcat :: (a -> String) -> String -> Either String [a] -> Either String String
    mapconcat _ _ (Left s) = Left s
    mapconcat f m (Right l) = Right $ concat (intersperse m (map f l))

getBabel :: [String] -> String
getBabel lins =  cleanLine (concat (intersperse " " region))
    where
    region = hd ++ [(head tl)]
    (hd, tl) = span (\x -> (matchRegex reEnd x) == Nothing)
                    (dropWhile (\x -> (matchRegex reStart x) == Nothing) lins)

    cleanLine x = maybe "can't parse this language" head $ matchRegex reLine x

    reLine =  mkRegex ".*padding:10px[^>]*>(.*)</div>.*"
    reStart = mkRegex ".*padding:10px[^>]*>"
    reEnd = mkRegex "</div>"

babelFish :: String -> String -> String -> IO String
babelFish inLang outLang string = do
    body <- either (\s -> error ("Error: " ++ s))
                   (\body -> return body) (mkBody inLang outLang string)
    lins <- readPage (proxy config) uri (mkPost uri body) body
    return (getBabel lins)
    where
    uri = (fromJust $ parseURI babelFishURL)


------------------------------------------------------------------------

split :: Char -> Int -> String -> [String]
split c i s =
        let fn 0 t = t:[]
            fn j t = let (xs,ys) = break (== c) t
                     in case ys of
                        [] -> xs:[]
                        _  -> xs: fn (j-1) (tail ys)
        in fn (i-1) s
