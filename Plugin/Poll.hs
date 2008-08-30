{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Module: Vote
-- | Support for voting
-- |
-- | License: lGPL
-- |
-- | added by Kenneth Hoste (boegel), 22/11/2005
-- |  inspiration: Where plugin (thanks shapr,dons)
module Plugin.Poll (theModule) where

import Plugin hiding (choice)
import qualified Data.ByteString.Char8 as P
import qualified Data.Map as M

$(plugin "Vote")

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
type VoteWriter        = VoteState -> LB ()
-- type Vote m a          = ModuleT VoteState m a

------------------------------------------------------------------------
-- Define a serialiser

voteSerial :: Serial VoteState
voteSerial = Serial (Just . showPacked) (Just . readPacked)

------------------------------------------------------------------------

instance Module VoteModule VoteState where
    moduleCmds     _ = ["poll-list"
                       ,"poll-show"
                       ,"poll-add"
                       ,"choice-add"
                       ,"vote"
                       ,"poll-result"
                       ,"poll-close"
                       ,"poll-remove"]
--
   -- todo, should @vote foo automagically add foo as a possibility?
    moduleHelp   _ s = case s of
        "poll-list"     -> "poll-list                   Shows all current polls"
        "poll-show"     -> "poll-show <poll>            Shows all choices for some poll"
        "poll-add"      -> "poll-add <name>             Adds a new poll, with no candidates"
        "choice-add"    -> "choice-add <poll> <choice>  Adds a new choice to the given poll"
        "vote"          -> "vote <poll> <choice>        Vote for <choice> in <poll>"
        "poll-result"   -> "poll-result <poll>          Show result for given poll"
        "poll-close"    -> "poll-close <poll>           Closes a poll"
        "poll-remove"   -> "poll-remove <poll>          Removes a poll"

    moduleDefState _  = return M.empty
    moduleSerialize _ = Just voteSerial

    process_ _ "poll-list" _ = do
        result <- withMS $ \factFM writer -> processCommand factFM writer "poll-list" []
        return [result]

    process_ _ _ [] = return ["Missing argument. Check @help <vote-cmd> for info."]

    process_ _ cmd dat = do
        result <- withMS $ \factFM writer -> processCommand factFM writer cmd (words dat)
        return [result]

------------------------------------------------------------------------

processCommand :: VoteState -> VoteWriter -> String -> [String] -> LB String
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

addPoll :: VoteState -> VoteWriter -> String -> LB String
addPoll fm writer poll =
    case M.lookup (P.pack poll) fm of
        Nothing -> do writer $ M.insert (P.pack poll) newPoll fm
                      return $ "Added new poll: " ++ show poll
        Just _  -> return $ "Poll " ++ show poll ++
                            " already exists, choose another name for your poll"

addChoice :: VoteState -> VoteWriter -> String -> String -> LB String
addChoice fm writer poll choice = case M.lookup (P.pack poll) fm of
    Nothing -> return $ "No such poll: " ++ show poll
    Just _  -> do writer $ M.update (appendPoll choice) (P.pack poll) fm
                  return $ "New candidate " ++ show choice ++
                           ", added to poll " ++ show poll ++ "."

vote :: VoteState -> VoteWriter -> String -> String -> LB String
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

removePoll :: VoteState -> VoteWriter -> String -> LB String
removePoll fm writer poll = case M.lookup (P.pack poll) fm of
    Just (True,_)  -> return "Poll should be closed before you can remove it."
    Just (False,_) -> do writer $ M.delete (P.pack poll) fm
                         return $ "poll " ++ show poll ++ " removed."
    Nothing        -> return $ "No such poll: " ++ show poll

closePoll :: VoteState -> VoteWriter -> String -> LB String
closePoll fm writer poll = case M.lookup (P.pack poll) fm of
    Nothing     -> return $ "No such poll: " ++ show poll
    Just (_,p)  -> do writer $ M.update (const (Just (False,p))) (P.pack poll) fm
                      return $ "Poll " ++ show poll ++ " closed."

