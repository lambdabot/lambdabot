{-# LANGUAGE TypeFamilies #-}
-- | Module: Poll
-- | Support for voting
-- |
-- | License: lGPL
-- |
-- | added by Kenneth Hoste (boegel), 22/11/2005
-- |  inspiration: Where plugin (thanks shapr,dons)
module Lambdabot.Plugin.Social.Poll (pollPlugin) where

import Lambdabot.Plugin
import qualified Data.ByteString.Char8 as P
import Data.List
import qualified Data.Map as M

newPoll :: Poll
newPoll = (True,[])

appendPoll :: Choice -> Poll -> (Maybe Poll)
appendPoll choice (o,ls) = Just (o,(choice,0):ls)

voteOnPoll :: Poll -> Choice -> (Poll,String)
voteOnPoll (o,poll) choice =
    if any (\(x,_) -> x == choice) poll
        then ((o,map (\(c,n) ->
                    if c == choice then (c,n+1)
                                   else (c,n)) poll)
                                        ,"voted on " ++ show choice)
        else ((o,poll),show choice ++ " is not currently a candidate in this poll")

------------------------------------------------------------------------

type Count             = Int
type Choice            = P.ByteString
type PollName          = P.ByteString
type Poll              = (Bool, [(Choice, Count)])
type VoteState         = M.Map PollName Poll
type VoteWriter        = VoteState -> Cmd Vote ()
type Vote              = ModuleT VoteState LB

------------------------------------------------------------------------
-- Define a serialiser

voteSerial :: Serial VoteState
voteSerial = Serial (Just . showPacked) (Just . readPacked)

------------------------------------------------------------------------

pollPlugin :: Module (M.Map PollName Poll)
pollPlugin = newModule
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
        , (command "poll-reset")
            { help = say "poll-reset <poll>           Resets votes and reopens a poll"
            , process = process_ "poll-reset"
            }
        ]

    , moduleDefState  = return M.empty
    , moduleSerialize = Just voteSerial
    }

process_ :: [Char] -> [Char] -> Cmd Vote ()
process_ cmd [] = say ("Missing argument. Check @help " ++ cmd ++ " for info.")
process_ cmd dat = do
    result <- withMS $ \fm writer ->
        processCommand fm writer cmd (map P.pack (words dat))
    say result

------------------------------------------------------------------------

processCommand :: VoteState -> VoteWriter -> String -> [P.ByteString] -> Cmd Vote String
processCommand fm writer cmd dat = case cmd of

    -- show all current polls
    "poll-list"  -> return $ listPolls fm

    -- show candidates
    "poll-show"    -> return $ case dat of
                        [poll] -> showPoll fm poll
                        _ -> "usage: @poll-show <poll>"

    -- declare a new poll
    "poll-add"     -> case dat of
                        [poll] -> addPoll fm writer poll
                        _ -> return "usage: @poll-add <poll>   with \"ThisTopic\" style names"

    "choice-add"   -> case dat of
                        [poll,choice] -> addChoice fm writer poll choice
                        _ -> return "usage: @choice-add <poll> <choice>"

    "vote"          -> case dat of
                        [poll,choice] -> vote fm writer poll choice
                        _ -> return "usage: @vote <poll> <choice>"

    "poll-result"   -> return $ case dat of
                        [poll] -> showResult fm poll
                        _ -> "usage: @poll-result <poll>"

    "poll-close"    -> case dat of
                        [poll] -> closePoll fm writer poll
                        _ -> return "usage: @poll-close <poll>"

    "poll-remove"   -> case dat of
                        [poll] -> removePoll fm writer poll
                        _ -> return "usage: @poll-remove <poll>"

    "poll-reset"    -> case dat of
                        [poll] -> resetPoll fm writer poll
                        _ -> return "usage: @poll-reset <poll>"
    _ -> return "Unknown command."

------------------------------------------------------------------------

listPolls :: VoteState -> String
listPolls fm = show $ map fst (M.toList fm)

showPoll :: VoteState -> PollName -> String
showPoll fm poll =
    case M.lookup poll fm of
        Nothing -> "No such poll: " ++ show poll ++ " Use @poll-list to see the available polls."
        Just p  -> show $ map fst (snd p)

addPoll :: VoteState -> VoteWriter -> PollName -> Cmd Vote String
addPoll fm writer poll =
    case M.lookup poll fm of
        Nothing -> do writer $ M.insert poll newPoll fm
                      return $ "Added new poll: " ++ show poll
        Just _  -> return $ "Poll " ++ show poll ++
                            " already exists, choose another name for your poll"

addChoice :: VoteState -> VoteWriter -> PollName -> Choice -> Cmd Vote String
addChoice fm writer poll choice = case M.lookup poll fm of
    Nothing -> return $ "No such poll: " ++ show poll
    Just _  -> do writer $ M.update (appendPoll choice) poll fm
                  return $ "New candidate " ++ show choice ++
                           ", added to poll " ++ show poll ++ "."

vote :: VoteState -> VoteWriter -> PollName -> Choice -> Cmd Vote String
vote fm writer poll choice = case M.lookup poll fm of
    Nothing          -> return $ "No such poll:" ++ show poll
    Just (False,_)   -> return $ "The "++ show poll ++ " poll is closed, sorry !"
    Just p@(True,_)  -> do let (np,msg) = voteOnPoll p choice
                           writer $ M.update (const (Just np)) poll fm
                           return msg

showResult :: VoteState -> PollName -> String
showResult fm poll = case M.lookup poll fm of
    Nothing     -> "No such poll: "  ++ show poll
    Just (o,p)  -> "Poll results for " ++ show poll ++ " (" ++ status o ++ "): "
                   ++ (concat $ intersperse ", " $ map ppr p)
        where
            status s | s         = "Open"
                     | otherwise = "Closed"
            ppr (x,y) = show x ++ "=" ++ show y

removePoll :: VoteState -> VoteWriter -> PollName -> Cmd Vote String
removePoll fm writer poll = case M.lookup poll fm of
    Just (True,_)  -> return "Poll should be closed before you can remove it."
    Just (False,_) -> do writer $ M.delete poll fm
                         return $ "poll " ++ show poll ++ " removed."
    Nothing        -> return $ "No such poll: " ++ show poll

closePoll :: VoteState -> VoteWriter -> PollName -> Cmd Vote String
closePoll fm writer poll = case M.lookup poll fm of
    Nothing     -> return $ "No such poll: " ++ show poll
    Just (_,p)  -> do writer $ M.update (const (Just (False,p))) poll fm
                      return $ "Poll " ++ show poll ++ " closed."

resetPoll :: VoteState -> VoteWriter -> PollName -> Cmd Vote String
resetPoll fm writer poll = case M.lookup poll fm of
    Just (_, vs)   -> do let np = (True, map (\(c, _) -> (c, 0)) vs)
                         writer $ M.update (const (Just np)) poll fm
                         return $ "Poll " ++ show poll ++ " reset."
    Nothing        -> return $ "No such poll: " ++ show poll
