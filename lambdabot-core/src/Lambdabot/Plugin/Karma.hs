-- | Karma
module Lambdabot.Plugin.Karma (theModule) where

import Lambdabot.Compat.FreenodeNick
import Lambdabot.Plugin
import qualified Lambdabot.NickEq as E

import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Text.Printf

type KarmaState = M.Map Nick Integer
type Karma = ModuleT KarmaState LB

theModule :: Module KarmaState
theModule = newModule
    { moduleCmds = return
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

    , moduleDefState  = return $ M.empty
    , moduleSerialize = Just freenodeNickMapSerial

    -- nick++($| )
    , contextual = \text -> withMsg $ \_ -> do
        sender <- getSender

        let ws          = words text
            decs        = match "--"
            incs        = match "++"
            match m     = mapM readNick . filter okay . map (reverse . drop 2)
                        . filter (isPrefixOf m) . map reverse $ ws
            okay x      = not (elem x badNicks || any (`isPrefixOf` x) badPrefixes)
            -- Special cases.  Ignore the null nick.  C must also be ignored
            -- because C++ and C-- are languages.
            badNicks    = ["", "C", "c", "notepad"]
            -- More special cases, to ignore Perl code.
            badPrefixes = ["$", "@", "%"]

        mapM_ (changeKarma (-1) sender) =<< decs
        mapM_ (changeKarma   1  sender) =<< incs
    }

doCmd :: Integer -> String -> Cmd Karma ()
doCmd dk rest = do
    sender <- getSender
    case words rest of
      []       -> say "usage @karma(+|-) nick"
      (nick:_) -> do
          nick' <- readNick nick
          changeKarma dk sender nick' >>= say

------------------------------------------------------------------------

tellKarma :: Nick -> E.Polynick -> Cmd Karma ()
tellKarma sender nick = do
    lookup' <- lb E.lookupMononickMap
    karma <- (sum . map snd . lookup' nick) `fmap` readMS
    nickStr <- withMsg (return . flip E.showPolynick nick)
    say $ concat [if E.mononickToPolynick sender == nick then "You have" else nickStr ++ " has"
                   ," a karma of "
                   ,show karma]

listKarma :: Cmd Karma ()
listKarma = do
    ks <- M.toList `fmap` readMS
    let ks' = sortBy (\(_,e) (_,e') -> e' `compare` e) ks
    flip mapM_ ks' $ \(k,e) -> do
        k' <- showNick k
        say (printf " %-20s %4d" k' e)

changeKarma :: Integer -> Nick -> Nick -> Cmd Karma String
changeKarma km sender nick
    | map toLower (nName nick) == "java" && km > 0 = do
        me <- getLambdabotName
        changeKarma (-km) me sender
    | sender == nick = return "You can't change your own karma, silly."
    | otherwise      = do
        nickStr <- showNick nick
        withMS $ \fm write -> do
            let fm' = M.insertWith (+) nick km fm
            let karma = fromMaybe 0 $ M.lookup nick fm'
            write fm'
            return (fmt nickStr km (show karma))
        where
            fmt n v k | v < 0     = n ++ "'s karma lowered to "    ++ k ++ "."
                      | v == 0    = n ++ "'s karma unchanged at "  ++ k ++ "."
                      | otherwise = n ++ "'s karma raised to "     ++ k ++ "."
