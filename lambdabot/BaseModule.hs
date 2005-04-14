module BaseModule (theModule) where

import Config                   (config, Config(name, autojoin))
import IRC
import Util                     (debugStrLn, breakOnGlue, split, closest)
import qualified Map as M       (insert, delete)

import Data.List                (isPrefixOf)
import Text.Regex               (mkRegex, matchRegexAll)
import Control.Monad.State      (MonadState(..))

newtype BaseModule = BaseModule ()

theModule :: MODULE
theModule = MODULE $ BaseModule ()

instance Module BaseModule () where
    moduleHelp  _ _   = return "base module"
    moduleCmds      _ = return []
    process _ _ _ _ _ = return ()
    moduleInit _
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



doUNKNOWN :: Callback
doUNKNOWN msg
    = debugStrLn $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

doIGNORE :: Callback
doIGNORE msg
  = debugStrLn $ show msg
 --   = debugStrLn $ "IGNORING> <" ++ msgPrefix msg ++
--      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)


doPING :: Callback
doPING msg
    = debugStrLn $ "ERROR> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (msgParams msg)

doNOTICE :: Callback
doNOTICE msg
    = debugStrLn $ "NOTICE: " ++ show (msgParams msg)

doJOIN :: Callback
doJOIN msg
  = do let (_, _:loc) = breakOnGlue ":" (head (msgParams msg))
       s <- get
       put (s { ircChannels = M.insert  (mkCN loc) "[currently unknown]" (ircChannels s)}) -- the empty topic causes problems
       ircGetTopic loc -- initialize topic

doPART :: Callback
doPART msg
  = do  let loc = head (msgParams msg)
        s <- get
        put (s { ircChannels = M.delete (mkCN loc) (ircChannels s) }) -- this must be a bug

doNICK :: Callback
doNICK msg
  = doIGNORE msg

doMODE :: Callback
doMODE msg
  = doIGNORE msg


doTOPIC :: Callback
doTOPIC msg
    = do let loc = (head (msgParams msg))
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ msgParams msg) (ircChannels s)})

doRPL_WELCOME :: Callback
doRPL_WELCOME _msg = mapM_ ircJoin (autojoin config)

doQUIT :: Callback
doQUIT msg = doIGNORE msg

{-
doRPL_YOURHOST :: Callback
doRPL_YOURHOST _msg = return ()

doRPL_CREATED :: Callback
doRPL_CREATED _msg = return ()

doRPL_MYINFO :: Callback
doRPL_MYINFO _msg = return ()
-}

doRPL_BOUNCE :: Callback
doRPL_BOUNCE _msg = debugStrLn "BOUNCE!"

{-
doRPL_STATSCONN :: Callback
doRPL_STATSCONN _msg = return ()

doRPL_LUSERCLIENT :: Callback
doRPL_LUSERCLIENT _msg = return ()

doRPL_LUSEROP :: Callback
doRPL_LUSEROP _msg = return ()

doRPL_LUSERUNKNOWN :: Callback
doRPL_LUSERUNKNOWN _msg = return ()

doRPL_LUSERCHANNELS :: Callback
doRPL_LUSERCHANNELS _msg = return ()

doRPL_LUSERME :: Callback
doRPL_LUSERME _msg = return ()

doRPL_LOCALUSERS :: Callback
doRPL_LOCALUSERS _msg = return ()

doRPL_GLOBALUSERS :: Callback
doRPL_GLOBALUSERS _msg = return ()
-}

doRPL_TOPIC :: Callback
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of msgParams
    = do let loc = (msgParams msg) !! 1
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last $ msgParams msg) (ircChannels s) })

{-
doRPL_NAMREPLY :: Callback
doRPL_NAMREPLY _msg = return ()

doRPL_ENDOFNAMES :: Callback
doRPL_ENDOFNAMES _msg = return ()

doRPL_MOTD :: Callback
doRPL_MOTD _msg = return ()

doRPL_MOTDSTART :: Callback
doRPL_MOTDSTART _msg = return ()

doRPL_ENDOFMOTD :: Callback
doRPL_ENDOFMOTD _msg = return ()
-}

doPRIVMSG :: Callback
doPRIVMSG msg = doPRIVMSG' (name config) msg

--
-- | What does the bot respond to?
--
doPRIVMSG' :: String -> IRCMessage -> IRC ()
doPRIVMSG' myname msg
  | myname `elem` targets
    = let (cmd, params) = breakOnGlue " " text
      in doPersonalMsg cmd (dropWhile (== ' ') params)

  | (myname ++ ":") `isPrefixOf` text
    = let Just wholeCmd = maybeCommand myname text
	  (cmd, params) = breakOnGlue " " wholeCmd
      in doPublicMsg cmd (dropWhile (==' ') params)

  | ("@" `isPrefixOf` text)
    = let (cmd, params) = breakOnGlue " " (dropWhile (==' ') text)
      in doPublicMsg cmd (dropWhile (==' ') params)

  | otherwise = doIGNORE msg

  where
    alltargets = head (msgParams msg)
    targets = split "," alltargets
    text = tail (head (tail (msgParams msg)))

    doPersonalMsg ('@':cmd) rest = do
        allcmds <- getDictKeys ircCommands
        let cmd' | (n,s) <- closest cmd allcmds, n <= 3 = s
                 | otherwise                            = cmd
        withModule ircCommands cmd'
            (ircPrivmsg who "Sorry, I don't know that command.") 
            (\m -> do 
               debugStrLn (show msg)
               handleIrc (ircPrivmsg who) (process m msg who cmd' rest))
      where (who, _) = breakOnGlue "!" (msgPrefix msg)

    doPersonalMsg _ _
      = do  let (who, _) = breakOnGlue "!" (msgPrefix msg)
            ircPrivmsg who "Sorry, don't understand"
            doUNKNOWN msg

    -- external modules are called in this next chunk
    doPublicMsg ('@':cmd) rest = do
        allcmds <- getDictKeys ircCommands
        let cmd' | (n,s) <- closest cmd allcmds, n <= 3 = s
                 | otherwise                            = cmd
        withModule ircCommands cmd'
            (ircPrivmsg alltargets ("Unknown command, try @listcommands.")) 
            (\m -> do
              debugStrLn (show msg)
              handleIrc (ircPrivmsg alltargets) 
                        (process m msg alltargets cmd' rest))

    doPublicMsg _ _ = ircPrivmsg alltargets ("Not a command (no @).")

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text =
    let re = mkRegex (nm ++ "[.:,]*[[:space:]]*")
    in case matchRegexAll re text of
      Nothing -> Nothing
      Just (_, _, cmd, _) -> Just cmd

