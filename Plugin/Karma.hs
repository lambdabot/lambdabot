{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Karma
module Plugin.Karma (theModule) where

import Plugin
import qualified Message as Msg (nick, Nick, Message, showNick, readNick, lambdabotName, nName)
import qualified NickEq as E
import qualified Data.Map as M
import Text.Printf

$(plugin "Karma")

type KarmaState = M.Map Msg.Nick Integer
type Karma m a = ModuleT KarmaState m a

instance Module KarmaModule KarmaState where

    moduleCmds _ = ["karma", "karma+", "karma-", "karma-all"]
    moduleHelp _ "karma"     = "karma <polynick>. Return a person's karma value"
    moduleHelp _ "karma+"    = "karma+ <nick>. Increment someone's karma"
    moduleHelp _ "karma-"    = "karma- <nick>. Decrement someone's karma"
    moduleHelp _ "karma-all" = "karma-all. List all karma"

    moduleDefState  _ = return $ M.empty
    moduleSerialize _ = Just mapSerial

    process      _ _ _ "karma-all" _ = listKarma
    process      _ msg _ "karma" rest = tellKarma msg sender nick'
        where sender = Msg.nick msg
              nick' = case words rest of
                        []       -> E.mononickToPolynick sender
                        (nick:_) -> E.readPolynick msg nick
    process      _ msg _ cmd rest =
        case words rest of
          []       -> return [ "usage @karma(+|-) nick" ]
          (nick:_) -> do
              let nick' = Msg.readNick msg nick
              case cmd of
                 "karma+"    -> changeKarma msg 1    sender nick'
                 "karma-"    -> changeKarma msg (-1) sender nick'
                 _        -> error "KarmaModule: can't happen"
        where sender = Msg.nick msg

    -- ^nick++($| )
    contextual   _ msg _ text = do
        mapM_ (changeKarma msg (-1) sender) decs
        mapM_ (changeKarma msg   1  sender) incs
        return []
      where
        sender      = Msg.nick msg
        ws          = words text
        decs        = match "--"
        incs        = match "++"
        match m     = map (Msg.readNick msg) . filter okay . map (reverse . drop 2)
                    . filter (isPrefixOf m) . map reverse $ ws
        okay x      = not (elem x badNicks || any (`isPrefixOf` x) badPrefixes)
        -- Special cases.  Ignore the null nick.  C must also be ignored
        -- because C++ and C-- are languages.
        badNicks    = ["", "C", "c", "notepad"]
        -- More special cases, to ignore Perl code.
        badPrefixes = ["$", "@", "%"]

------------------------------------------------------------------------

tellKarma :: Msg.Message m => m -> Msg.Nick -> E.Polynick -> Karma LB [String]
tellKarma msg sender nick = do
    lookup' <- lift E.lookupMononickMap
    karma <- (sum . map snd . lookup' nick) `fmap` readMS
    return [concat [if E.mononickToPolynick sender == nick then "You have" else E.showPolynick msg nick ++ " has"
                   ," a karma of "
                   ,show karma]]

listKarma :: Karma LB [String]
listKarma = do
    ks <- M.toList `fmap` readMS
    let ks' = sortBy (\(_,e) (_,e') -> e' `compare` e) ks
    return $ (:[]) . unlines $ map (\(k,e) -> printf " %-20s %4d" (show k) e :: String) ks'

changeKarma :: Msg.Message m => m -> Integer -> Msg.Nick -> Msg.Nick -> Karma LB [String]
changeKarma msg km sender nick
  | map toLower (Msg.nName nick) == "java" && km == 1 = changeKarma msg (-km) (Msg.lambdabotName msg) sender
  | sender == nick = return ["You can't change your own karma, silly."]
  | otherwise      = withMS $ \fm write -> do
      let fm' = M.insertWith (+) nick km fm
      let karma = fromMaybe 0 $ M.lookup nick fm'
      write fm'
      return [fmt (Msg.showNick msg nick) km (show karma)]
          where fmt n v k | v < 0     = n ++ "'s karma lowered to " ++ k ++ "."
                          | otherwise = n ++ "'s karma raised to " ++ k ++ "."
