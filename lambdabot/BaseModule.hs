module BaseModule (baseModule) where

import BotConfig
import IRC
import Util
import qualified Map as M

import Data.List
import Data.Maybe
import Control.Monad.State
import Text.Regex


newtype BaseModule = BaseModule ()

baseModule :: BaseModule
baseModule = BaseModule ()

instance Module BaseModule () where
    moduleName      _ = return "base"
    moduleHelp  _ _   = return "base module"
    commands        _ = return []
    process _ _ _ _ _ = return ()
    moduleInit _m
	= do ircSignalConnect "PING" 	doPING
	     ircSignalConnect "NOTICE" 	doNOTICE
	     ircSignalConnect "PART" 	doPART
	     ircSignalConnect "JOIN"    doJOIN
	     ircSignalConnect "NICK" 	doNICK
	     ircSignalConnect "MODE" 	doMODE
	     ircSignalConnect "TOPIC" 	doTOPIC
	     ircSignalConnect "QUIT" 	doQUIT
	     ircSignalConnect "PRIVMSG" doPRIVMSG
	     ircSignalConnect "001"	doRPL_WELCOME

	  {- ircSignalConnect "002"	doRPL_YOURHOST
	     ircSignalConnect "003"	doRPL_CREATED
	     ircSignalConnect "004"	doRPL_MYINFO -}

	     ircSignalConnect "005" 	doRPL_BOUNCE

	  {- ircSignalConnect "250"	doRPL_STATSCONN
	     ircSignalConnect "251"     doRPL_LUSERCLIENT
	     ircSignalConnect "252"     doRPL_LUSEROP
	     ircSignalConnect "253"     doRPL_LUSERUNKNOWN
	     ircSignalConnect "254"     doRPL_LUSERCHANNELS
	     ircSignalConnect "255"     doRPL_LUSERME
	     ircSignalConnect "265"	doRPL_LOCALUSERS
	     ircSignalConnect "266"	doRPL_GLOBALUSERS -}

	     ircSignalConnect "332"	doRPL_TOPIC

	  {- ircSignalConnect "353"	doRPL_NAMRELY
	     ircSignalConnect "366"     doRPL_ENDOFNAMES
	     ircSignalConnect "372"	doRPL_MOTD
	     ircSignalConnect "375"	doRPL_MOTDSTART
	     ircSignalConnect "376"     doRPL_ENDOFMOTD -}



doUNKNOWN :: IRCMessage -> IRC ()
doUNKNOWN msg
    = debugStrLn $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

doIGNORE :: IRCMessage -> IRC ()
doIGNORE msg
  = debugStrLn $ show msg
 --   = debugStrLn $ "IGNORING> <" ++ msgPrefix msg ++
--      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)


doPING :: IRCMessage -> IRC ()
doPING msg
    = debugStrLn $ "ERROR> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

doNOTICE :: IRCMessage -> IRC ()
doNOTICE msg
    = debugStrLn $ "NOTICE: " ++ show (msgParams msg)

doJOIN :: IRCMessage -> IRC ()
doJOIN msg
  = do let (_, _:loc) = breakOnGlue ":" (head (msgParams msg))
       s <- get
       put (s { ircChannels = M.insert  (mkCN loc) "[currently unknown]" (ircChannels s)}) -- the empty topic causes problems
       ircGetTopic loc -- initialize topic

doPART :: IRCMessage -> IRC ()
doPART msg
  = do  let loc = head (msgParams msg)
        s <- get
        put (s { ircChannels = M.delete (mkCN loc) (ircChannels s) }) -- this must be a bug

doNICK :: IRCMessage -> IRC ()
doNICK msg
  = doIGNORE msg

doMODE :: IRCMessage -> IRC ()
doMODE msg
  = doIGNORE msg


doTOPIC :: IRCMessage -> IRC ()
doTOPIC msg
    = do let loc = (head (msgParams msg))
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ msgParams msg) (ircChannels s)})

doRPL_WELCOME :: IRCMessage -> IRC ()
doRPL_WELCOME _msg
  = do autojoins <- getAutojoins
       mapM_ ircJoin autojoins

doQUIT :: IRCMessage -> IRC ()
doQUIT msg 
  = doIGNORE msg

{-
doRPL_YOURHOST :: IRCMessage -> IRC ()
doRPL_YOURHOST _msg = return ()

doRPL_CREATED :: IRCMessage -> IRC ()
doRPL_CREATED _msg = return ()

doRPL_MYINFO :: IRCMessage -> IRC ()
doRPL_MYINFO _msg = return ()
-}

doRPL_BOUNCE :: IRCMessage -> IRC ()
doRPL_BOUNCE _msg = debugStrLn "BOUNCE!"

{-
doRPL_STATSCONN :: IRCMessage -> IRC ()
doRPL_STATSCONN _msg = return ()

doRPL_LUSERCLIENT :: IRCMessage -> IRC ()
doRPL_LUSERCLIENT _msg = return ()

doRPL_LUSEROP :: IRCMessage -> IRC ()
doRPL_LUSEROP _msg = return ()

doRPL_LUSERUNKNOWN :: IRCMessage -> IRC ()
doRPL_LUSERUNKNOWN _msg = return ()

doRPL_LUSERCHANNELS :: IRCMessage -> IRC ()
doRPL_LUSERCHANNELS _msg = return ()

doRPL_LUSERME :: IRCMessage -> IRC ()
doRPL_LUSERME _msg = return ()

doRPL_LOCALUSERS :: IRCMessage -> IRC ()
doRPL_LOCALUSERS _msg = return ()

doRPL_GLOBALUSERS :: IRCMessage -> IRC ()
doRPL_GLOBALUSERS _msg = return ()
-}

doRPL_TOPIC :: IRCMessage -> IRC ()
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of msgParams
    = do let loc = (msgParams msg) !! 1
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last $ msgParams msg) (ircChannels s) })

{-
doRPL_NAMREPLY :: IRCMessage -> IRC ()
doRPL_NAMREPLY _msg = return ()

doRPL_ENDOFNAMES :: IRCMessage -> IRC ()
doRPL_ENDOFNAMES _msg = return ()

doRPL_MOTD :: IRCMessage -> IRC ()
doRPL_MOTD _msg = return ()

doRPL_MOTDSTART :: IRCMessage -> IRC ()
doRPL_MOTDSTART _msg = return ()

doRPL_ENDOFMOTD :: IRCMessage -> IRC ()
doRPL_ENDOFMOTD _msg = return ()
-}

doPRIVMSG :: IRCMessage -> IRC ()
doPRIVMSG msg = do myname <- getMyname
                   doPRIVMSG' myname msg

doPRIVMSG' :: String -> IRCMessage -> IRC ()
doPRIVMSG' myname msg
  | myname `elem` targets
    = let (cmd, params) = breakOnGlue " " text
      in doPersonalMsg cmd (dropWhile (== ' ') params)
  | myname `isPrefixOf` text
    = let Just wholeCmd = maybeCommand myname text
	  (cmd, params) = breakOnGlue " " wholeCmd
      in doPublicMsg cmd (dropWhile (==' ') params)
  | ("@" `isPrefixOf` text)
    = let (cmd, params) = breakOnGlue " " (dropWhile (==' ') text)
      in doPublicMsg cmd (dropWhile (==' ') params)
  | otherwise
    = doIGNORE msg
  where
    alltargets = head (msgParams msg)
    targets = split "," alltargets
    text = tail (head (tail (msgParams msg)))

    doPersonalMsg ('@':cmd) rest = withModule ircCommands cmd
        (ircPrivmsg who "Sorry, I don't know that command.") (\m -> do 
           debugStrLn (show msg)
           handleIrc (ircPrivmsg who) (process m msg who cmd rest)
        )
      where (who, _) = breakOnGlue "!" (msgPrefix msg)

    doPersonalMsg _ _
      = do  let (who, _) = breakOnGlue "!" (msgPrefix msg)
            ircPrivmsg who "Sorry, don't understand"
            doUNKNOWN msg
    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) rest = withModule ircCommands cmd (do
          myname' <- getMyname
          ircPrivmsg alltargets ("Sorry, I don't know the command \"" ++
                                 cmd ++ "\", try \"" ++ myname' ++ ": @listcommands\"")
        ) (\m -> do
          debugStrLn (show msg)
          handleIrc (ircPrivmsg alltargets) (process m msg alltargets cmd rest)
        )

    doPublicMsg _ _
      = do myname' <- getMyname
           ircPrivmsg alltargets ("Sorry, I'm not a very smart bot yet, try \""
                                        ++ myname' ++ ": @listcommands\"")

maybeCommand :: String -> String -> Maybe String
maybeCommand name text =
    let re = mkRegex (name ++ "[.:,]*[[:space:]]*")
    in case matchRegexAll re text of
      Nothing -> Nothing
      Just (_, _, cmd, _) -> Just cmd

