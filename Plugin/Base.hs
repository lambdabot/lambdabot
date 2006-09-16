--
-- | Lambdabot base module. Controls message send and receive
--
module Plugin.Base (theModule) where

import Plugin

import IRC (IrcMessage, timeReply, errShowMsg)
import Message (getTopic, nick, joinChannel, body)

import qualified Data.Map as M   (insert, delete)

import Control.Monad.State  (MonadState(..), when, gets)

import GHC.IOBase           (Exception(NoMethodError))
import Data.IORef

-- valid command prefixes
commands :: [String]
commands  = commandPrefixes config

PLUGIN Base

type BaseState = GlobalPrivate () ()
type Base a = ModuleT BaseState LB a

instance Module BaseModule BaseState where
    moduleDefState  _ = return $ mkGlobalPrivate 20 ()
    moduleInit _ = do
             ircSignalConnect "PING"    doPING
             bindModule1 doNOTICE >>= ircSignalConnect "NOTICE"
             ircSignalConnect "PART"    doPART
             ircSignalConnect "JOIN"    doJOIN
             ircSignalConnect "NICK"    doNICK
             ircSignalConnect "MODE"    doMODE
             ircSignalConnect "TOPIC"   doTOPIC
             ircSignalConnect "QUIT"    doQUIT
             bindModule1 doPRIVMSG >>= ircSignalConnect "PRIVMSG"
             ircSignalConnect "001"     doRPL_WELCOME

          {- ircSignalConnect "002"     doRPL_YOURHOST
             ircSignalConnect "003"     doRPL_CREATED
             ircSignalConnect "004"     doRPL_MYINFO -}

             ircSignalConnect "005"     doRPL_BOUNCE

          {- ircSignalConnect "250"     doRPL_STATSCONN
             ircSignalConnect "251"     doRPL_LUSERCLIENT
             ircSignalConnect "252"     doRPL_LUSEROP
             ircSignalConnect "253"     doRPL_LUSERUNKNOWN
             ircSignalConnect "254"     doRPL_LUSERCHANNELS
             ircSignalConnect "255"     doRPL_LUSERME
             ircSignalConnect "265"     doRPL_LOCALUSERS
             ircSignalConnect "266"     doRPL_GLOBALUSERS -}

             ircSignalConnect "332"     doRPL_TOPIC

          {- ircSignalConnect "353"     doRPL_NAMRELY
             ircSignalConnect "366"     doRPL_ENDOFNAMES
             ircSignalConnect "372"     doRPL_MOTD
             ircSignalConnect "375"     doRPL_MOTDSTART
             ircSignalConnect "376"     doRPL_ENDOFMOTD -}

doIGNORE :: Callback
doIGNORE msg = debugStrLn $ show msg
 --   = debugStrLn $ "IGNORING> <" ++ msgPrefix msg ++
--      "> [" ++ msgCommand msg ++ "] " ++ show (body msg)


doPING :: Callback
doPING msg
    = debugStrLn $ errShowMsg msg

-- If this is a "TIME" then we need to pass it over to the localtime plugin
-- otherwise, dump it to stdout
doNOTICE :: IrcMessage -> Base ()
doNOTICE msg =
  if isCTCPTimeReply
     then do
        -- bind implicit params to Localtime module. boo on implict params :/
  --    withModule ircModules 
  --               "Localtime"
  --               (error "Plugin/Base: no Localtime plugin? So I can't handle CTCP time messges")
  --               (\_ -> doPRIVMSG (timeReply msg))

          -- need to say which module to run the privmsg in

          doPRIVMSG (timeReply msg)

     else debugStrLn $ "NOTICE: " ++ show (body msg)
    where
      isCTCPTimeReply = ":\SOHTIME" `isPrefixOf` (last (body msg)) 

doJOIN :: Callback
doJOIN msg
  = do s <- get
       put (s { ircChannels = M.insert  (mkCN loc) "[currently unknown]" (ircChannels s)}) -- the empty topic causes problems
       send_ $ getTopic loc -- initialize topic
   where (_, aloc) = breakOnGlue ":" (head (body msg))
         loc       = case aloc of 
                        [] -> [] 
                        _  -> tail aloc

doPART :: Callback
doPART msg
  = when (name config == nick msg) $ do  
        let loc = head (body msg)
        s <- get
        put (s { ircChannels = M.delete (mkCN loc) (ircChannels s) })

doNICK :: Callback
doNICK msg
  = doIGNORE msg

doMODE :: Callback
doMODE msg
  = doIGNORE msg


doTOPIC :: Callback
doTOPIC msg
    = do let loc = (head (body msg))
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ body msg) (ircChannels s)})

doRPL_WELCOME :: Callback
doRPL_WELCOME _msg = mapM_ (send_ . joinChannel) (autojoin config)

doQUIT :: Callback
doQUIT msg = doIGNORE msg

doRPL_BOUNCE :: Callback
doRPL_BOUNCE _msg = debugStrLn "BOUNCE!"

doRPL_TOPIC :: Callback
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of body
    = do let loc = (body msg) !! 1
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last $ body msg) (ircChannels s) })

doPRIVMSG :: IrcMessage -> Base ()
doPRIVMSG msg = do
    -- work out if we're in offline mode or not.
--  debugStrLn (show msg)
    doPRIVMSG' (name config) msg

--
-- | What does the bot respond to?
--
doPRIVMSG' :: String -> IRC.IrcMessage -> Base ()
doPRIVMSG' myname msg
  | myname `elem` targets
    = let (cmd, params) = breakOnGlue " " text
      in doPersonalMsg cmd (dropWhile (== ' ') params)

  | flip any ":," $ \c -> (myname ++ [c]) `isPrefixOf` text
    = let Just wholeCmd = maybeCommand myname text
          (cmd, params) = breakOnGlue " " wholeCmd
      in doPublicMsg cmd (dropWhile (==' ') params)

  | (commands `arePrefixesOf` text)
  && length text > 1
  && (text !! 1 /= ' ') -- elem of prefixes
  && (not (commands `arePrefixesOf` [text !! 1]))
    = let (cmd, params) = breakOnGlue " " (dropWhile (==' ') text)
      in doPublicMsg cmd (dropWhile (==' ') params)

  | otherwise =  doContextualMsg text

  where
    alltargets = head (body msg)
    targets = split "," alltargets
    text = tail (head (tail (body msg)))
    who = nick msg

    doPersonalMsg s r
        | commands `arePrefixesOf` s = doMsg (tail s) r who
        | s `elem` (evalPrefixes config)    = doMsg "run"   r who -- TODO
        | otherwise                  = lift $ doIGNORE msg -- contextual?

    doPublicMsg s r
        | commands `arePrefixesOf` s                = doMsg (tail s)        r alltargets
        | (evalPrefixes config) `arePrefixesWithSpaceOf` s = doMsg "run" r alltargets -- TODO
        | otherwise                                 = lift $ doIGNORE msg -- contextual?

    --
    -- normal commands.
    --
    -- check privledges, do any spell correction, dispatch, handling
    -- possible timeouts.
    --
    -- todo, refactor
    --
    doMsg cmd rest towhere = do
        let ircmsg = ircPrivmsg towhere
        allcmds <- getDictKeys ircCommands
        let ms      = filter (isPrefixOf cmd) allcmds
        case ms of
            [s] -> docmd s                  -- a unique prefix
            _ | cmd `elem` ms -> docmd cmd  -- correct command (usual case)
            _ | otherwise     -> case closests cmd allcmds of
                  (n,[s]) | n < e ,  ms == [] -> docmd s -- unique edit match
                  (n,ss)  | n < e || ms /= []            -- some possibilities
                          -> lift . ircmsg . Just $ "Maybe you meant: "++showClean(nub(ms++ss))
                  _ -> docmd cmd         -- no prefix, edit distance too far
        where
            e = 3   -- edit distance cut off. Seems reasonable for small words

            docmd cmd' = do
              act <- bindModule0 . withPS towhere $ \_ _ -> do
                withModule ircCommands cmd'   -- Important. 
                    (ircPrivmsg towhere (Just "Unknown command, try @list"))
                    (\m -> do
                        name'   <- getName
                        privs   <- gets ircPrivCommands
                        let illegal = disabledCommands config
                        ok      <- liftM2 (||) (return $ cmd' `notElem` (privs ++ illegal))
                                               (lift $ checkPrivs msg)
                        if not ok
                          then lift $ ircPrivmsg towhere $ Just "Not enough privileges"
                          else catchIrc
                            (do mstrs <- catchError
                                    (process m msg towhere cmd' rest)
                                    (\ex -> case (ex :: IRCError) of -- dispatch
                                                (IRCRaised (NoMethodError _)) ->
                                                    process_ m cmd' rest
                                                _ -> throwError ex)
                                case filter (not. null) mstrs of
                                    [] -> lift $ ircPrivmsg towhere Nothing
                                    _  -> lift $ mapM_ (ircPrivmsg towhere . Just) mstrs)

                            (lift . ircPrivmsg towhere . Just .
                                ((name' ++ " module failed: ") ++) . show))
              lift $ forkLB act
    --
    -- contextual messages are all input that isn't an explicit command.
    -- they're passed to all modules (todo, sounds inefficient) for
    -- scanning, and any that implement 'contextual' will reply.
    --
    -- we try to run the contextual functions from all modules, on every
    -- non-command. better hope this is efficient.
    --
    -- Note how we catch any plugin errors here, rather than letting
    -- them bubble back up to the mainloop
    --
    doContextualMsg r = lift $ do
        x <- io $ newIORef []       -- track if any output was made, for offline mode
        withAllModules ( \m -> do
            act <- bindModule0 ( do
                            ms <- contextual m msg alltargets r
                            io $ modifyIORef x (null ms :)
                            lift $ mapM_ (ircPrivmsg alltargets . Just) ms )
            name' <- getName
            lift $ catchIrc act (debugStrLn . (name' ++) .
                (" module failed in contextual handler: " ++) . show)
            )
        rs <- io $ readIORef x
        when (all id rs) $ ircPrivmsg alltargets Nothing

------------------------------------------------------------------------

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text = case matchRegexAll re text of
      Nothing -> Nothing
      Just (_, _, cmd, _) -> Just cmd
    where re = mkRegex (nm ++ "[.:,]*[[:space:]]*")

--
-- And stuff we don't care about
--

{-
doRPL_YOURHOST :: Callback
doRPL_YOURHOST _msg = return ()

doRPL_CREATED :: Callback
doRPL_CREATED _msg = return ()

doRPL_MYINFO :: Callback
doRPL_MYINFO _msg = return ()

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

doUNKNOWN :: Callback
doUNKNOWN msg
    = debugStrLn $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (body msg)

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
