module BaseModule where

import BotConfig
import IRC
import Control.Monad.State
import Data.FiniteMap
import Util

newtype BaseModule = BaseModule ()

baseModule = BaseModule ()

instance Module BaseModule where
    moduleName      _ = return "base"
    moduleSticky    _ = False
    commands        _ = return []
    process _ _ _ _ _ = return ()
    moduleInit m 
	= do s <- get
	     let cbsFM = ircCallbacks s 
	     ircSignalConnect "PING" 	doPING
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
       put (s { ircChannels = addToFM (ircChannels s) (mkCN loc) "[currently unknown]" }) -- the empty topic causes problems
       ircGetTopic loc -- initialize topic

doPART :: IRCMessage -> IRC ()
doPART msg
  = do  let loc = head (msgParams msg)
        s <- get
        put (s { ircChannels = delFromFM (ircChannels s) (mkCN loc) }) -- this must be a bug

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
         put (s { ircChannels = addToFM (ircChannels s) (mkCN loc) (tail $ head $ tail $ msgParams msg) })

doRPL_WELCOME :: IRCMessage -> IRC ()
doRPL_WELCOME msg
  = do autojoins <- getAutojoins
       joinMany autojoins
  where
    joinMany :: [String] -> IRC ()
    joinMany [] = return ()
    joinMany (c:cs) = ircJoin c >> joinMany cs

doQUIT :: IRCMessage -> IRC ()
doQUIT msg 
  = doIGNORE msg

doRPL_YOURHOST :: IRCMessage -> IRC ()
doRPL_YOURHOST msg = return ()

doRPL_CREATED :: IRCMessage -> IRC ()
doRPL_CREATED msg = return ()

doRPL_MYINFO :: IRCMessage -> IRC ()
doRPL_MYINFO msg = return ()

doRPL_BOUNCE :: IRCMessage -> IRC ()
doRPL_BOUNCE msg = debugStrLn "BOUNCE!"

doRPL_STATSCONN :: IRCMessage -> IRC ()
doRPL_STATSCONN msg = return ()

doRPL_LUSERCLIENT :: IRCMessage -> IRC ()
doRPL_LUSERCLIENT msg = return ()

doRPL_LUSEROP :: IRCMessage -> IRC ()
doRPL_LUSEROP msg = return ()

doRPL_LUSERUNKNOWN :: IRCMessage -> IRC ()
doRPL_LUSERUNKNOWN msg = return ()

doRPL_LUSERCHANNELS :: IRCMessage -> IRC ()
doRPL_LUSERCHANNELS msg = return ()

doRPL_LUSERME :: IRCMessage -> IRC ()
doRPL_LUSERME msg = return ()

doRPL_LOCALUSERS :: IRCMessage -> IRC ()
doRPL_LOCALUSERS msg = return ()

doRPL_GLOBALUSERS :: IRCMessage -> IRC ()
doRPL_GLOBALUSERS msg = return ()

doRPL_TOPIC :: IRCMessage -> IRC ()
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of msgParams
    = do let loc = (msgParams msg) !! 1
         s <- get
         put (s { ircChannels = addToFM (ircChannels s) (mkCN loc) (tail $ last $ msgParams msg) })

doRPL_NAMREPLY :: IRCMessage -> IRC ()
doRPL_NAMREPLY msg = return ()

doRPL_ENDOFNAMES :: IRCMessage -> IRC ()
doRPL_ENDOFNAMES msg = return ()

doRPL_MOTD :: IRCMessage -> IRC ()
doRPL_MOTD msg = return ()

doRPL_MOTDSTART :: IRCMessage -> IRC ()
doRPL_MOTDSTART msg = return ()

doRPL_ENDOFMOTD :: IRCMessage -> IRC ()
doRPL_ENDOFMOTD msg = return ()

doPRIVMSG :: IRCMessage -> IRC ()
doPRIVMSG msg = do myname <- getMyname
                   doPRIVMSG' myname msg

doPRIVMSG' :: String -> IRCMessage -> IRC ()
doPRIVMSG' myname msg
  | myname `elem` targets
    = let (cmd, params) = breakOnGlue " " text
      in doPersonalMsg cmd (dropWhile (== ' ') params)
  | (myname ++ ":") `prefix` text
    = let (cmd, params) = breakOnGlue " "
                    (dropWhile (==' ') (after (myname ++ ":") text))
      in doPublicMsg cmd (dropWhile (==' ') params)
  | ("@" `prefix` text)
    = let (cmd, params) = breakOnGlue " " (dropWhile (==' ') text)
      in doPublicMsg cmd (dropWhile (==' ') params)
  | otherwise
    = doIGNORE msg
  where
    alltargets = head (msgParams msg)
    targets = split "," alltargets
    text = tail (head (tail (msgParams msg)))

    doPersonalMsg ('@':cmd) rest
        = do let (who, _) = breakOnGlue "!" (msgPrefix msg)
             maybecmd <- gets (\s -> lookupFM (ircCommands s) cmd)
             case maybecmd of
               Just (MODULE m) -> do debugStrLn (show msg) 
                                     handleIrc (ircPrivmsg who) (process m msg who cmd rest)
               Nothing -> ircPrivmsg who "Sorry, I don't know that command."
    doPersonalMsg _ _
      = do  let (who, _) = breakOnGlue "!" (msgPrefix msg)
            ircPrivmsg who "Sorry, don't understand"
            doUNKNOWN msg
    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) rest
     = do maybecmd <- gets (\s -> lookupFM (ircCommands s) cmd)
          case maybecmd of
            Just (MODULE m) -> do debugStrLn (show msg)
                                  handleIrc (ircPrivmsg alltargets) (process m msg alltargets cmd rest)
            Nothing         -> do myname <- getMyname
                                  ircPrivmsg alltargets ("Sorry, I don't know the command \"" ++ cmd ++ "\", try \"" ++ myname ++ ": @listcommands\"")
    doPublicMsg _ _
      = do myname <- getMyname
           ircPrivmsg alltargets ("Sorry, I'm not a very smart bot yet, try \"" ++ myname ++ ": @listcommands\"")
                    
after :: String -> String -> String
after [] ys     = dropWhile (==' ') ys
after (x:xs) [] = error "after: (:) [] case"
after (x:xs) (y:ys)
  | x == y    = after xs ys
  | otherwise = error "after: /= case"

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys)
  = x == y && prefix xs ys


