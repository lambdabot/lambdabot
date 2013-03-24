{-# LANGUAGE TypeFamilies #-}
-- | Module: Vote
-- | Support for voting
-- |
-- | License: lGPL
-- |
-- | added by Kenneth Hoste (boegel), 22/11/2005
-- |  inspiration: Where plugin (thanks shapr,dons)
module Lambdabot.Plugin.Poll (theModule) where

import Lambdabot.Plugin
import qualified Data.ByteString.Char8 as P
import Data.List
import qualified Data.Map as M

newPoll :: Poll
newPoll = (True,[])

appendPoll :: String -> Poll -> (Maybe Poll)
appendPoll choice (o,ls) = Just (o,(choice,0):ls)

voteOnPoll :: Poll -> String -> (Poll,String)
voteOnPoll (o,poll) choice =
    if any (\(x,_) -> x == choice) poll
        then ((o,map (\(c,n) ->
                    if c == choice then (c,n+1)
                                   else (c,n)) poll)
                                        ,"voted on " ++ show choice)
        else ((o,poll),show choice ++ " is not currently a candidate in this poll")

------------------------------------------------------------------------

type Count             = Int
type Candidate         = String
type PollName          = P.ByteString
type Poll              = (Bool, [(Candidate, Count)])
type VoteState         = M.Map PollName Poll
type VoteWriter        = VoteState -> Cmd Vote ()
type Vote              = ModuleT VoteState LB

------------------------------------------------------------------------
-- Define a serialiser

voteSerial :: Serial VoteState
voteSerial = Serial (Just . showPacked) (Just . readPacked)

------------------------------------------------------------------------

theModule :: Module (M.Map PollName Poll)
theModule = newModule
    { moduleCmds = return
        [ (command "poll-list")
            { help = say "poll-list                   Shows all current polls"
            , process = \_ -> do
                result <- withMS $ \factFM writer -> processCommand factFM writer "poll-list" []
                say result
            }
        , (command "poll-show")
            { help = say "poll-show <poll>            Shows all choices for some poll"
            , process = process_ "poll-show"
            }
        , (command "poll-add")
            { help = say "poll-add <name>             Adds a new poll, with no candidates"
            , process = process_ "poll-add"
            }
        , (command "choice-add")
            { help = say "choice-add <poll> <choice>  Adds a new choice to the given poll"
            , process = process_ "choice-add"
            }
        , (command "vote")
            -- todo, should @vote foo automagically add foo as a possibility?
            { help = say "vote <poll> <choice>        Vote for <choice> in <poll>"
            , process = process_ "vote"
            }
        , (command "poll-result")
            { help = say "poll-result <poll>          Show result for given poll"
            , process = process_ "poll-result"
            }
        , (command "poll-close")
            { help = say "poll-close <poll>           Closes a poll"
            , process = process_ "poll-close"
            }
        , (command "poll-remove")
            { help = say "poll-remove <poll>          Removes a poll"
            , process = process_ "poll-remove"
            }
        ]

    , moduleDefState  = return M.empty
    , moduleSerialize = Just voteSerial
    }

process_ :: [Char] -> [Char] -> Cmd Vote ()
process_ cmd [] = say ("Missing argument. Check @help " ++ cmd ++ " for info.")
process_ cmd dat = do
    result <- withMS $ \fm writer -> processCommand fm writer cmd (words dat)
    say result

------------------------------------------------------------------------

processCommand :: VoteState -> VoteWriter -> String -> [String] -> Cmd Vote String
processCommand fm writer cmd dat = case cmd of

    -- show all current polls
    "poll-list"  -> return $ listPolls fm

    -- show candidates
    "poll-show"    -> return $ case length dat of
                        1 -> showPoll fm (head dat)
                        _ -> "usage: @poll-show <poll>"

    -- declare a new poll
    "poll-add"     -> case length dat of
                        1 -> addPoll fm writer (head dat)
                        _ -> return "usage: @poll-add <poll>   with \"ThisTopic\" style names"

    "choice-add"   -> case length dat of
                        2 -> addChoice fm writer (head dat) (last dat)
                        _ -> return "usage: @choice-add <poll> <choice>"

    "vote"          -> case length dat of
                        2 -> vote fm writer (head dat) (last dat)
                        _ -> return "usage: @vote <poll> <choice>"

    "poll-result"   -> return $ case length dat of
                        1 -> showResult fm (head dat)
                        _ -> "usage: @poll-result <poll>"

    "poll-close"    -> case length dat of
                        1 -> closePoll fm writer (head dat)
                        _ -> return "usage: @poll-close <poll>"

    "poll-remove"   -> case length dat of
                        1 -> removePoll fm writer (head dat)
                        _ -> return "usage: @poll-remove <poll>"

    _ -> return "Unknown command."

------------------------------------------------------------------------

listPolls :: VoteState -> String
listPolls fm = show $ map fst (M.toList fm)

showPoll :: VoteState -> String -> String
showPoll fm poll =
    case M.lookup (P.pack poll) fm of
        Nothing -> "No such poll: " ++ show poll ++ " Use @poll-list to see the available polls."
        Just p  -> show $ map fst (snd p)

addPoll :: VoteState -> VoteWriter -> String -> Cmd Vote String
addPoll fm writer poll =
    case M.lookup (P.pack poll) fm of
        Nothing -> do writer $ M.insert (P.pack poll) newPoll fm
                      return $ "Added new poll: " ++ show poll
        Just _  -> return $ "Poll " ++ show poll ++
                            " already exists, choose another name for your poll"

addChoice :: VoteState -> VoteWriter -> String -> String -> Cmd Vote String
addChoice fm writer poll choice = case M.lookup (P.pack poll) fm of
    Nothing -> return $ "No such poll: " ++ show poll
    Just _  -> do writer $ M.update (appendPoll choice) (P.pack poll) fm
                  return $ "New candidate " ++ show choice ++
                           ", added to poll " ++ show poll ++ "."

vote :: VoteState -> VoteWriter -> String -> String -> Cmd Vote String
vote fm writer poll choice = case M.lookup (P.pack poll) fm of
    Nothing          -> return $ "No such poll:" ++ show poll
    Just (False,_)   -> return $ "The "++ show poll ++ " poll is closed, sorry !"
    Just p@(True,_)  -> do let (np,msg) = voteOnPoll p choice
                           writer $ M.update (const (Just np)) (P.pack poll) fm
                           return msg

showResult :: VoteState -> String -> String
showResult fm poll = case M.lookup (P.pack poll) fm of
    Nothing     -> "No such poll: "  ++ show poll
    Just (o,p)  -> "Poll results for " ++ poll ++ " (" ++ (status o) ++ "): "
                   ++ (concat $ intersperse ", " $ map ppr p)
        where
            status s | s         = "Open"
                     | otherwise = "Closed"
            ppr (x,y) = x ++ "=" ++ show y

removePoll :: VoteState -> VoteWriter -> String -> Cmd Vote String
removePoll fm writer poll = case M.lookup (P.pack poll) fm of
    Just (True,_)  -> return "Poll should be closed before you can remove it."
    Just (False,_) -> do writer $ M.delete (P.pack poll) fm
                         return $ "poll " ++ show poll ++ " removed."
    Nothing        -> return $ "No such poll: " ++ show poll

closePoll :: VoteState -> VoteWriter -> String -> Cmd Vote String
closePoll fm writer poll = case M.lookup (P.pack poll) fm of
    Nothing     -> return $ "No such poll: " ++ show poll
    Just (_,p)  -> do writer $ M.update (const (Just (False,p))) (P.pack poll) fm
                      return $ "Poll " ++ show poll ++ " closed."
