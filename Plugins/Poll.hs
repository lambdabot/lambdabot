--
-- | Module: Vote
-- | Support for voting
-- |
-- | License: lGPL
-- |
-- | added by Kenneth Hoste (boegel), 22/11/2005
-- |  inspiration: Where plugin (thanks shapr,dons)
--
module Plugins.Poll (theModule) where

import Lambdabot
import LBState
import Serial
import Maybe
import qualified Data.FastPackedString as P
import qualified Map as M

newtype VoteModule = VoteModule ()

theModule :: MODULE
theModule = MODULE $ VoteModule ()

------------------------------------------------------------------------

newPoll :: String -> Poll
newPoll choice = (True,[(choice,0)])

appendPoll :: String -> Poll -> (Maybe Poll)
appendPoll choice (o,ls) = Just (o,(choice,0):ls)

voteOnPoll :: Poll -> String -> (Poll,String)
voteOnPoll (o,poll) choice = 
    if any (\(x,_) -> x == choice) poll
        then ((o,map (\(c,n) -> 
                    if c == choice then (c,n+1) 
                                   else (c,n)) poll)
                                        ,"voted on " ++ choice)
        else ((o,poll),"no such choice in this poll !")

------------------------------------------------------------------------

type Poll              = (Bool,[(String,Int)]) -- Faststrings are better to serialise
type VoteState         = M.Map P.FastString Poll
type VoteWriter        = VoteState -> LB ()
type Vote m a          = ModuleT VoteState m a

------------------------------------------------------------------------
-- Define a serialiser

voteSerial :: Serial VoteState
voteSerial = Serial (Just . showPacked) (Just . readPacked)

instance Packable (M.Map P.FastString Poll) where
    readPacked = M.fromList . readKV . P.lines
        where
          readKV :: [P.FastString] -> [(P.FastString,Poll)]
          readKV []         = []
          readKV (k:v:rest) = (k, (read . P.unpack) v) : readKV rest
          readKV _          = error "Vote.readPacked: parse failed"

    showPacked m = P.unlines . concatMap (\(k,v) -> [k,P.pack . show $ v]) $ M.toList m

------------------------------------------------------------------------

instance Module VoteModule VoteState where
    moduleCmds     _ = return ["poll-browse"
                              ,"poll-show"
                              ,"poll-add"
                              ,"choice-add"
                              ,"vote"
                              ,"poll-result"
                              ,"poll-close"
                              ,"poll-remove"]
--
    
    moduleHelp   _ s = return $ case s of
        "poll-browse"   -> "@poll-browse                 shows all current polls"
        "poll-show"     -> "@poll-show <poll>            shows all choices for some poll"
        "poll-add"      -> "@poll-add <name> <choice>    adds a new poll with a single choice"
        "choice-add"    -> "@choice-add <poll> <choice>  adds a new choice to the given poll"
        "vote"          -> "@vote <poll> <choice>        vote for <choice> in <poll>"
        "poll-result"   -> "@poll-result <poll>          show result for given poll"
        "poll-close"    -> "@poll-close <poll>           closes a poll"
        "poll-remove"   -> "@poll-remove <poll>          removes a poll"
        _               -> "vote plugin - run elections on various topics"
    
    moduleDefState _  = return M.empty
    moduleSerialize _ = Just voteSerial
    
    process _ _ target "poll-browse" _ = do 
        result <- withMS $ \factFM writer -> processCommand factFM writer "poll-browse" []
        ircPrivmsg target result

    process _ _ target _ [] = do 
        result <- return  "Don't forget the parameters ! Check @help <vote-cmd> for info."
        ircPrivmsg target result

    process _ _ target cmd dat = do
        result <- withMS $ \factFM writer -> processCommand factFM writer cmd (words dat)
        ircPrivmsg target result

processCommand :: VoteState -> VoteWriter -> String -> [String] -> Vote LB String

processCommand fm writer cmd dat = case cmd of
    "poll-browse"  -> return $ browsePolls fm

    "poll-show"    -> return $ if length dat >= 1 
                        then showPoll fm (head dat) 
                        else "usage: @poll-show <poll>"

    "poll-add"     -> if length dat >= 2
                        then addPoll fm writer (head dat) (dat !! 1) 
                        else return "usage: @poll-add <poll> <choice>"

    "choice-add"   -> if length dat >=2 
                        then addChoice fm writer (head dat) (dat !! 1)
                        else return "usage: @choice-add <poll> <choice>"

    "vote"          -> if length dat >= 2 
                        then vote fm writer (head dat) (dat !! 1)
                        else return "usage: @vote <poll> <choice>"

    "poll-result"   -> return $ if length dat >= 1
                        then showResult fm (head dat)
                        else "usage: @poll-result <poll>"

    "poll-close"    -> if length dat >= 1 
                        then closePoll fm writer (head dat)
                        else return "usage: @poll-close <poll>"

    "poll-remove"   -> if length dat >= 1 
                        then removePoll fm writer (head dat)
                        else return "usage: @poll-remove <poll>"

    _ -> return "Unknown command."

------------------------------------------------------------------------

browsePolls :: VoteState -> String
browsePolls fm = show $ map fst (M.toList fm)

showPoll :: VoteState -> String -> String
showPoll fm poll = 
    case M.lookup (P.pack poll) fm of
        Nothing -> "No such poll ! Use @poll-browse to see the available polls."
        Just p  -> show $ map fst (snd p)

addPoll :: VoteState -> VoteWriter -> String -> String -> Vote LB String
addPoll fm writer poll choice = 
    case M.lookup (P.pack poll) fm of
        Nothing -> do writer $ M.insert (P.pack poll) (newPoll choice) fm
                      return $ "added new poll: " ++ poll ++ "."
        Just _  -> return "Poll already exists, choose another name for your poll"

addChoice :: VoteState -> VoteWriter -> String -> String -> Vote LB String
addChoice fm writer poll choice = case M.lookup (P.pack poll) fm of
    Nothing -> return "No such poll !"
    Just _  -> do writer $ M.update (appendPoll choice) (P.pack poll) fm
                  return $ "new choice (" ++ choice ++ ") added to poll '" ++ poll ++ "'."

vote :: VoteState -> VoteWriter -> String -> String -> Vote LB String
vote fm writer poll choice = case M.lookup (P.pack poll) fm of
    Nothing          -> return "No such poll !"
    Just (False,_)   -> return "Poll is closed, sorry !"
    Just p@(True,_)  -> do let (np,msg) = voteOnPoll p choice
                           writer $ M.update (const (Just np)) (P.pack poll) fm
                           return msg

showResult :: VoteState -> String -> String
showResult fm poll = case M.lookup (P.pack poll) fm of
    Nothing     -> "No such poll !"
    Just (o,p)  -> (show o) ++ (show p)

removePoll :: VoteState -> VoteWriter -> String -> Vote LB String
removePoll fm writer poll = case M.lookup (P.pack poll) fm of
    Just (True,_)  -> return "Poll should be closed before removing !"
    Just (False,_) -> do writer $ M.delete (P.pack poll) fm
                         return $ "poll " ++ poll ++ " removed."
    Nothing        -> return "Error while removing poll"

closePoll :: VoteState -> VoteWriter -> String -> Vote LB String
closePoll fm writer poll = case M.lookup (P.pack poll) fm of
    Nothing     -> return "No such poll !"
    Just (_,p)  -> do writer $ M.update (const (Just (False,p))) (P.pack poll) fm
                      return $ "poll " ++ poll ++ " closed."
