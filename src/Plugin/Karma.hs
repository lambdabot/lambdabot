{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | Karma
module Plugin.Karma (theModule) where

import Plugin
import qualified Lambdabot.Message as Msg (Nick, Message, showNick, readNick, lambdabotName, nName)
import qualified Lambdabot.NickEq as E
import qualified Data.Map as M
import Text.Printf

plugin "Karma"

type KarmaState = M.Map Msg.Nick Integer

instance Module KarmaModule where
    
    type ModuleState KarmaModule = KarmaState

    moduleCmds _ =
        [ (command "karma")
            { help = say "karma <polynick>. Return a person's karma value"
            , process = \rest -> withMsg $ \msg -> do
                sender <- getSender
                tellKarma sender $ case words rest of
                    []       -> E.mononickToPolynick sender
                    (nick:_) -> E.readPolynick msg nick

            }
        , (command "karma+")
            { help = say "karma+ <nick>. Increment someone's karma"
            , process = doCmd 1
            }
        , (command "karma-")
            { help = say "karma- <nick>. Decrement someone's karma"
            , process = doCmd (-1)
            }
        , (command "karma-all")
            { help = say "karma-all. List all karma"
            , process = const listKarma
            }
        ]

    moduleDefState  _ = return $ M.empty
    moduleSerialize _ = Just mapSerial

    -- ^nick++($| )
    contextual _ text = withMsg $ \msg -> do
        sender <- getSender
        
        let ws          = words text
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
        
        mapM_ (lift . changeKarma msg (-1) sender) decs
        mapM_ (lift . changeKarma msg   1  sender) incs

doCmd dk rest = withMsg $ \msg -> do
    sender <- getSender
    case words rest of
      []       -> say "usage @karma(+|-) nick"
      (nick:_) -> do
          nick' <- readNick nick
          lift (changeKarma msg dk sender nick') >>= mapM_ say

------------------------------------------------------------------------

tellKarma :: Msg.Nick -> E.Polynick -> Cmd Karma ()
tellKarma sender nick = do
    lookup' <- lift (lift E.lookupMononickMap)
    karma <- (sum . map snd . lookup' nick) `fmap` lift readMS
    nickStr <- withMsg (return . flip E.showPolynick nick)
    say $ concat [if E.mononickToPolynick sender == nick then "You have" else nickStr ++ " has"
                   ," a karma of "
                   ,show karma]

listKarma :: Cmd Karma ()
listKarma = do
    ks <- M.toList `fmap` lift readMS
    let ks' = sortBy (\(_,e) (_,e') -> e' `compare` e) ks
    mapM_ (\(k,e) -> say (printf " %-20s %4d" (show k) e)) ks'

changeKarma :: Msg.Message m => m -> Integer -> Msg.Nick -> Msg.Nick -> Karma [String]
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
