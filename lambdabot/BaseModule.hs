{-# OPTIONS -fglasgow-exts #-}
module BaseModule where

import BotConfig
import IRC
import Util
import qualified Map as M

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Text.Regex


newtype BaseModule = BaseModule ()

baseModule :: BaseModule
baseModule = BaseModule ()

instance Module BaseModule () where
    moduleName      _ = return "base"
    moduleSticky    _ = False
    moduleHelp  _ _   = return "base module"
    commands        _ = return []
    process _ _ _ _ _ = return ()
    moduleInit _m
	= do s <- get
	     let _cbsFM = ircCallbacks s
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



doUNKNOWN :: MonadIRC m => IRCMessage -> m ()
doUNKNOWN msg
    = debugStrLn $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

doIGNORE :: MonadIRC m => IRCMessage -> m ()
doIGNORE msg
  = debugStrLn $ show msg
 --   = debugStrLn $ "IGNORING> <" ++ msgPrefix msg ++
--      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)


doPING :: MonadIRC m => IRCMessage -> m ()
doPING msg
    = debugStrLn $ "ERROR> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

doNOTICE :: MonadIRC m => IRCMessage -> m ()
doNOTICE msg
    = debugStrLn $ "NOTICE: " ++ show (msgParams msg)

doJOIN :: MonadIRC m => IRCMessage -> m ()
doJOIN msg
  = do let (_, _:loc) = breakOnGlue ":" (head (msgParams msg))
       s <- get
       put (s { ircChannels = M.insert  (mkCN loc) "[currently unknown]" (ircChannels s)}) -- the empty topic causes problems
       ircGetTopic loc -- initialize topic

doPART :: MonadIRC m => IRCMessage -> m ()
doPART msg
  = do  let loc = head (msgParams msg)
        s <- get
        put (s { ircChannels = M.delete (mkCN loc) (ircChannels s) }) -- this must be a bug

doNICK :: MonadIRC m => IRCMessage -> m ()
doNICK msg
  = doIGNORE msg

doMODE :: MonadIRC m => IRCMessage -> m ()
doMODE msg
  = doIGNORE msg


doTOPIC :: MonadIRC m => IRCMessage -> m ()
doTOPIC msg
    = do let loc = (head (msgParams msg))
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ msgParams msg) (ircChannels s)})

doRPL_WELCOME :: MonadIRC m => IRCMessage -> m ()
doRPL_WELCOME _msg
  = do autojoins <- getAutojoins
       mapM_ ircJoin autojoins

doQUIT :: MonadIRC m => IRCMessage -> m ()
doQUIT msg 
  = doIGNORE msg

doRPL_YOURHOST :: MonadIRC m => IRCMessage -> m ()
doRPL_YOURHOST _msg = return ()

doRPL_CREATED :: MonadIRC m => IRCMessage -> m ()
doRPL_CREATED _msg = return ()

doRPL_MYINFO :: MonadIRC m => IRCMessage -> m ()
doRPL_MYINFO _msg = return ()

doRPL_BOUNCE :: MonadIRC m => IRCMessage -> m ()
doRPL_BOUNCE _msg = debugStrLn "BOUNCE!"

doRPL_STATSCONN :: MonadIRC m => IRCMessage -> m ()
doRPL_STATSCONN _msg = return ()

doRPL_LUSERCLIENT :: MonadIRC m => IRCMessage -> m ()
doRPL_LUSERCLIENT _msg = return ()

doRPL_LUSEROP :: MonadIRC m => IRCMessage -> m ()
doRPL_LUSEROP _msg = return ()

doRPL_LUSERUNKNOWN :: MonadIRC m => IRCMessage -> m ()
doRPL_LUSERUNKNOWN _msg = return ()

doRPL_LUSERCHANNELS :: MonadIRC m => IRCMessage -> m ()
doRPL_LUSERCHANNELS _msg = return ()

doRPL_LUSERME :: MonadIRC m => IRCMessage -> m ()
doRPL_LUSERME _msg = return ()

doRPL_LOCALUSERS :: MonadIRC m => IRCMessage -> m ()
doRPL_LOCALUSERS _msg = return ()

doRPL_GLOBALUSERS :: MonadIRC m => IRCMessage -> m ()
doRPL_GLOBALUSERS _msg = return ()

doRPL_TOPIC :: MonadIRC m => IRCMessage -> m ()
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of msgParams
    = do let loc = (msgParams msg) !! 1
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last $ msgParams msg) (ircChannels s) })

doRPL_NAMREPLY :: MonadIRC m => IRCMessage -> m ()
doRPL_NAMREPLY _msg = return ()

doRPL_ENDOFNAMES :: MonadIRC m => IRCMessage -> m ()
doRPL_ENDOFNAMES _msg = return ()

doRPL_MOTD :: MonadIRC m => IRCMessage -> m ()
doRPL_MOTD _msg = return ()

doRPL_MOTDSTART :: MonadIRC m => IRCMessage -> m ()
doRPL_MOTDSTART _msg = return ()

doRPL_ENDOFMOTD :: MonadIRC m => IRCMessage -> m ()
doRPL_ENDOFMOTD _msg = return ()

doPRIVMSG :: MonadIRC m => IRCMessage -> m ()
doPRIVMSG msg = do myname <- getMyname
                   doPRIVMSG' myname msg

doPRIVMSG' :: MonadIRC m => String -> IRCMessage -> m ()
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

    doPersonalMsg ('@':cmd) rest
        = do let (who, _) = breakOnGlue "!" (msgPrefix msg)
             maybecmd <- gets (\s -> M.lookup cmd (ircCommands s))
             case maybecmd of
               Just (ModuleRef m ref) -> do 
                 debugStrLn (show msg)
                 liftIRC $ handleIrc (ircPrivmsg who) (process m msg who cmd rest `runReaderT` ref)
               Nothing -> ircPrivmsg who "Sorry, I don't know that command."
    doPersonalMsg _ _
      = do  let (who, _) = breakOnGlue "!" (msgPrefix msg)
            ircPrivmsg who "Sorry, don't understand"
            doUNKNOWN msg
    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) rest
     = do maybecmd <- gets (\s -> M.lookup cmd (ircCommands s))
          case maybecmd of
            Just (ModuleRef m ref) -> do 
              debugStrLn (show msg)
              liftIRC $ handleIrc (ircPrivmsg alltargets) (process m msg alltargets cmd rest `runReaderT` ref)
            Nothing         -> do
                myname' <- getMyname
                ircPrivmsg alltargets ("Sorry, I don't know the command \"" ++
                                        cmd ++ "\", try \"" ++ myname' ++ ": @listcommands\"")
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

