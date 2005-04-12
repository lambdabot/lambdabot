{-# OPTIONS -cpp #-}
--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- Copyright (c) 2004 Simon Winwood - http://www.cse.unsw.edu.au/~sjw
--

--
-- a translator module for lambdabot. requires the history buffer patch
-- to lambdabot for its history functionality. (in IRC.hs)
-- The history buffer adds privmsgs that come in onto a list, and msgs
-- that lambdabot writes back out, as well. This can later be used by
-- @last or @babel en de 10 to index sections of the history for profit
--

module BabelModule (theModule) where

import BabelBot.BabelFish       (shortLangs, babelFish)
import IRC
import Util                     (stdGetRandItem, mapSerializer)
import PosixCompat              (popen)
import qualified Map as M

import Data.List
import Data.Maybe               (fromMaybe)
import Control.Monad.Trans      (liftIO,MonadIO)

newtype BabelModule = BabelModule ()

theModule :: MODULE
theModule = MODULE $ BabelModule ()

type Quotes = M.Map String [String]

instance Module BabelModule Quotes where
        moduleName _            = "babel"

        moduleSerialize _       = Just mapSerializer
        moduleDefState  _       = return M.empty
       
        moduleHelp _ "babel"    = run_babel' ["help"] >>= return . concat
        moduleHelp _ "remember" = return "@remember <nick> quote - record some memorable phrase"
        moduleHelp _ "quote"    = return "@quote [nick] - quote somebody randomly"
        moduleHelp _ "timein"   = return "@timein <city>, report the local time in <city>"
        moduleHelp _ _          = return "@babel,@remember,@quote,@timein"

        moduleCmds _            = return ["babel", "remember", "quote", "timein" ]

        process _ _ src "babel" s     = run_babel src s
        process _ _ _src "remember"  s = run_remember  s
        process _ _ src "quote"     s = run_quote    src s
--      process _ _ src "last"  s     = run_last  src s

        -- totally unrelated :}
        process _ _ src "timein" s =
          if s == "help"
            then ircPrivmsg src "  http://www.timeanddate.com"
            else do (o,_,_) <- liftIO $ popen "timein" [s] Nothing
                    ircPrivmsg src $ "  " ++ o

        process _ _ _   _ _ = error "BabelBot: Invalid cmd"

--
-- The @babel command.
--
--      * translate a phrase
--      * translate last 'i' lines of history buffer
--      * translate a line of history indexed by a regex
--
-- TODO add range/context. i.e. !f-3 or 5-4
--
run_babel :: String -> String -> IRC ()

run_babel src s = do
        let cmd = split ' ' 3 s
        msg <- run_babel' cmd
        let msg' = map (\t -> "  "++t) msg
        ircPrivmsg src (unlines msg')

-- help msg
run_babel' :: (MonadIO m) => [[Char]] -> m [[Char]]
run_babel' ["help"] = return ["usage: babel lang lang phrase"]
run_babel' ["languages"] = return $ [show shortLangs]

-- translate last line
-- run_babel' [f,t] = doHistory f t 1

-- num-indexed history
-- regex-indexed hitory
-- phrase-immediate translation
run_babel' [f,t,i] 
--      | isNum i    = doHistory f t (read i)  
--      | isLookup i = doLookup f t (tail i)    -- chop '!' flag
        | otherwise  = do p <- liftIO $ babelFish f t i ; return [p]

run_babel' _ = return ["bzzt."]

{-
--
-- grab the history and translate it
--
doHistory :: String -> String -> Int -> IRC [String]
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
doLookup :: String -> String -> String -> IRC [String]
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
run_last :: String -> String -> IRC ()

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

run_remember :: String -> ModuleT Quotes IRC ()
run_remember str = do
        let (name,q') = break (== ' ') str
            q = if null q' then q' else tail q'
        fm <- readMS

        let ss  = fromMaybe [] (M.lookup name fm)
            fm' = M.insert name (q:ss) fm
        writeMS fm'

--
--  the @quote command, takes a user name to choose a random quote from
-- 
run_quote :: String -> String -> ModuleT Quotes IRC ()
run_quote target name = do
    fm <- readMS
    let qs' = M.lookup name fm
    (nm,qs) <- if name /= [] 
                then return (name,qs') -- (String, Maybe [String])

                else do (nm,rs') <- liftIO $ stdGetRandItem (M.toList fm) -- random person
                        return (nm, Just rs')

    case qs of
        Nothing   -> ircPrivmsg target $ nm ++ " hasn't said anything memorable"

        Just msgs -> do msg <- liftIO $ stdGetRandItem msgs
                        if name /= []
                            then ircPrivmsg target $ "  " ++ msg
                            else ircPrivmsg target $ nm++" says: " ++ msg



------------------------------------------------------------------------
--
split :: Char -> Int -> String -> [String]
split c i s = 
        let fn 0 t = t:[]
            fn j t = let (xs,ys) = break (== c) t
                     in case ys of
                        [] -> xs:[]
                        _  -> xs: fn (j-1) (tail ys)
        in fn (i-1) s

