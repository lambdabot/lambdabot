--
-- | Module: Vote
-- | Support for voting
-- |
-- | License: lGPL
-- |
-- | added by Kenneth Hoste (boegel), 22/11/2005
-- |  inspiration: Where plugin (thanks shapr,dons)
--
module Plugins.Vote (theModule) where

import Lambdabot
import LBState
--import Serial
import Maybe
import qualified Data.FastPackedString as P
import qualified Map as M

newtype VoteModule = VoteModule ()

theModule :: MODULE
theModule = MODULE $ VoteModule ()

type Poll = (Bool,[(String,Int)])

newPoll :: String -> Poll
newPoll choice = (True,[(choice,0)])

appendPoll :: String -> Poll -> (Maybe Poll)
appendPoll choice (o,ls) = Just (o,(choice,0):ls)

voteOnPoll :: Poll -> String -> (Poll,String)
voteOnPoll (o,poll) choice = if any (\(x,_) -> x == choice) poll
				then ((o,map (\(c,n) -> if c == choice then (c,n+1) else (c,n)) poll),"voted on " ++ choice)
				else ((o,poll),"no such choice in this poll !")


type VoteState         = M.Map P.FastString Poll
type VoteWriter        = VoteState -> LB ()
type Vote m a          = ModuleT VoteState m a

instance Module VoteModule VoteState where
    moduleCmds     _ = return ["browse-polls","show-poll","add-poll","add-choice","vote","show-result","close-poll","remove-poll"]
    
    moduleHelp   _ s = return $ case s of
    	"browse-polls" 	-> "@browse-polls, shows all current polls"
	"show-poll" 	-> "@show-poll <poll>, shows all choices for some poll"
	"add-poll" 	-> "@add-poll <name> <choice>, adds a new poll with a single choice"
	"add-choice" 	-> "@add-choice <poll> <choice>, adds a new choice to the given poll"
	"vote" 		-> "@vote <poll> <choice>, use this for voting on <choice> in given poll"
	"show-result" 	-> "@show-result <poll>, shows result for given poll"
	"close-poll" 	-> "@close-poll <poll>, closes a poll"
	"remove-poll" 	-> "@remove-poll <poll>, removes a poll"
    	_ 		-> "vote plugin - allows to cast a vote on some subject"
    
    moduleDefState _  = return $ M.empty
   -- moduleSerialize   = Just stdSerial -- this requires Show/Read instances for Poll
    
    process _ _ target "browse-polls" _ = do 
    	result <- withMS $ \factFM writer -> processCommand factFM writer "browse-polls" []
	ircPrivmsg target result
    process _ _ target _ [] = do 
    	result <- return  "Don't forget the parameters ! Check @help <vote-cmd> for info."
	ircPrivmsg target result
    process _ _ target cmd dat = do
    	result <- withMS $ \factFM writer -> processCommand factFM writer cmd (words dat)
	ircPrivmsg target result

processCommand :: VoteState -> VoteWriter -> String -> [String] -> Vote LB String

processCommand fm writer cmd dat = case cmd of
    	"browse-polls" 	-> return $ browsePolls fm
	"show-poll" 	-> return $ if (length dat >= 1) then showPoll fm (head dat) 
							 else "usage: @show-poll <poll>"
	"add-poll" 	-> if (length dat >= 2) then addPoll fm writer (head dat) (dat !! 1) 
						else return "usage: @add-poll <poll> <first choice>"
	"add-choice" 	-> if (length dat >=2 )  then addChoice fm writer (head dat) (dat !! 1)
						else return "usage: @add-choice <poll> <choice>"
	"vote" 		-> if (length dat >= 2) then vote fm writer (head dat) (dat !! 1)
						else return "usage: @vote <poll> <choice>"
	"show-result" 	-> return $ if (length dat >= 1) then showResult fm (head dat)
							 else "usage: @show-result <poll"
	"close-poll" 	-> if (length dat >= 1) then closePoll fm writer (head dat)
						else return "usage: @close-poll <poll>"
	"remove-poll" 	-> if (length dat >= 1) then removePoll fm writer (head dat)
						else return "usage: remove-poll <poll>"
	_           	-> return "Unknown command."

browsePolls :: VoteState -> String
browsePolls fm = show $ map fst polls
	where
		polls = M.toList fm
		

showPoll :: VoteState -> String -> String
showPoll fm poll = 
	case M.lookup (P.pack poll) fm of
		Nothing -> "No such poll ! Use @browse-polls to see the available polls."
		Just p  -> show $ map fst (snd p)

addPoll :: VoteState -> VoteWriter -> String -> String -> Vote LB String
addPoll fm writer poll choice = 
	case M.lookup (P.pack poll) fm of
		Nothing -> do 	writer $ M.insert (P.pack poll) (newPoll choice) fm
				return $ "added new poll: " ++ poll ++ "."
		Just _  -> do 	return "Poll already exists, choose another name for your poll"

addChoice :: VoteState -> VoteWriter -> String -> String -> Vote LB String
addChoice fm writer poll choice = case M.lookup (P.pack poll) fm of
	Nothing -> do return "No such poll !"
	Just _  -> do 	writer $ M.update (appendPoll choice) (P.pack poll) fm
			return $ "new choice (" ++ choice ++ ") added to poll '" ++ poll ++ "'."

vote :: VoteState -> VoteWriter -> String -> String -> Vote LB String
vote fm writer poll choice = case M.lookup (P.pack poll) fm of
	Nothing          -> do return "No such poll !"
	Just (False,_)   -> do return "Poll is closed, sorry !"
	Just p@(True,_)  -> do let (np,msg) = voteOnPoll p choice
		               writer $ M.update (const (Just np)) (P.pack poll) fm
		               return msg

showResult :: VoteState -> String -> String
showResult fm poll = case M.lookup (P.pack poll) fm of
	Nothing     -> "No such poll !"
	Just (o,p)  -> (show o) ++ (show p)

removePoll :: VoteState -> VoteWriter -> String -> Vote LB String
removePoll fm writer poll = case M.lookup (P.pack poll) fm of
	Nothing        -> do return "No such poll !"
	Just (True,_)  -> do return "Poll should be closed before removing !"
	Just (False,_) -> do writer $ M.delete (P.pack poll) fm
			     return $ "poll " ++ poll ++ " removed."

closePoll :: VoteState -> VoteWriter -> String -> Vote LB String
closePoll fm writer poll = case M.lookup (P.pack poll) fm of
	Nothing     -> do return "No such poll !"
	Just (_,p)  -> do writer $ M.update (const (Just (False,p))) (P.pack poll) fm
			  return $ "poll " ++ poll ++ " closed."
