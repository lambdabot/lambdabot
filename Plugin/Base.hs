--
-- | Lambdabot base module. Controls message send and receive
--
module Plugin.Base (theModule) where

import Plugin

import IRC (IrcMessage, timeReply, errShowMsg)
import Message (getTopic, nick, joinChannel, body)

import qualified Data.Map as M   (insert, delete)

import Control.Concurrent
import Control.Monad.State  (MonadState(..), when, gets)

import GHC.IOBase           (Exception(NoMethodError))

-- valid command prefixes
commands :: [String]
commands  = ["@","?"]

-- valid eval prefixes
evals  :: [String]
evals   = [">"]

PLUGIN Base

type BaseState = GlobalPrivate () ()

instance Module BaseModule BaseState where
    moduleDefState  _ = return $ mkGlobalPrivate 20 ()
    moduleInit _ = do
             ircSignalConnect "PING"    doPING
             ircSignalConnect "NOTICE"  doNOTICE
             ircSignalConnect "PART"    doPART
             ircSignalConnect "JOIN"    doJOIN
             ircSignalConnect "NICK"    doNICK
             ircSignalConnect "MODE"    doMODE
             ircSignalConnect "TOPIC"   doTOPIC
             ircSignalConnect "QUIT"    doQUIT
             ircSignalConnect "PRIVMSG" doPRIVMSG
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

{-
doUNKNOWN :: Callback
doUNKNOWN msg
    = debugStrLn $ "UNKNOWN> <" ++ msgPrefix msg ++
      "> [" ++ msgCommand msg ++ "] " ++ show (body msg)
-}

doIGNORE :: Callback
doIGNORE msg = debugStrLn $ show msg
 --   = debugStrLn $ "IGNORING> <" ++ msgPrefix msg ++
--      "> [" ++ msgCommand msg ++ "] " ++ show (body msg)


doPING :: Callback
doPING msg
    = debugStrLn $ errShowMsg msg

-- If this is a "TIME" then we need to pass it over to the localtime plugin
-- otherwise, dump it to stdout
doNOTICE :: ModState BaseState Callback
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
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of body
    = do let loc = (body msg) !! 1
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last $ body msg) (ircChannels s) })

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

doPRIVMSG :: ModState BaseState Callback
doPRIVMSG msg = do
    debugStrLn (show msg)
    doPRIVMSG' (name config) msg

arePrefixesOf :: [String] -> String -> Bool
arePrefixesOf = flip (any . flip isPrefixOf)

arePrefixesWithSpaceOf :: [String] -> String -> Bool
arePrefixesWithSpaceOf els str = any (flip isPrefixOf str) $ map (++" ") els

--
-- | What does the bot respond to?
--
doPRIVMSG' :: String -> IRC.IrcMessage -> ModuleT BaseState LB ()
doPRIVMSG' myname msg
  | myname `elem` targets
    = let (cmd, params) = breakOnGlue " " text
      in doPersonalMsg cmd (dropWhile (== ' ') params)

  | flip any ":," $ \c -> (myname ++ [c]) `isPrefixOf` text
    = let Just wholeCmd = maybeCommand myname text
          (cmd, params) = breakOnGlue " " wholeCmd
      in doPublicMsg cmd (dropWhile (==' ') params)

  | (commands `arePrefixesOf` text) && length text > 1 && (text !! 1 /= ' ') -- elem of prefixes
    = let (cmd, params) = breakOnGlue " " (dropWhile (==' ') text)
      in doPublicMsg cmd (dropWhile (==' ') params)

  -- special syntax for @run
  | evals `arePrefixesWithSpaceOf` text
    = let expr = drop 2 text
      in doPublicMsg "@run" (dropWhile (==' ') expr)

  -- send to all modules for contextual processing
  | otherwise = doContextualMsg text

  where
    alltargets = head (body msg)
    targets = split "," alltargets
    text = tail (head (tail (body msg)))
    who = nick msg

    doPersonalMsg s r | commands `arePrefixesOf` s = doMsg (tail s) r who
                      | s `elem` evals             = doMsg "run"   r who
                      | otherwise                  = doIGNORE msg

    doPublicMsg s r   | commands `arePrefixesOf` s          = doMsg (tail s)        r alltargets
                      | evals    `arePrefixesWithSpaceOf` s = doMsg "run" r alltargets
                      | otherwise                           = doIGNORE msg

    --
    -- normal commands.
    --
    -- check privledges, do any spell correction, dispatch, handling
    -- possible timeouts.
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
                          -> ircmsg . Just $ "Maybe you meant: "++showClean(nub(ms++ss))
                  _ -> docmd cmd         -- no prefix, edit distance too far
        where
            e = 3   -- edit distance cut off. Seems reasonable for small words

            docmd cmd' =
              forkLB $ withPS towhere $ \_ _ -> do
                withModule ircCommands cmd'   -- Important. 
                    (ircPrivmsg towhere (Just "Unknown command, try @list")) (\m -> do
                        privs <- gets ircPrivCommands
                        ok    <- if cmd' `notElem` privs
                                 then return True else checkPrivs msg
                        if not ok
                          then ircPrivmsg towhere $ Just "Not enough privileges"
                          else catchIrc

                            (do mstrs <- catchError -- :: m a -> (e -> m a) -> m a
                                        (process m msg towhere cmd' rest)
                                        (\ex -> case (ex :: IRCError) of -- dispatch
                                            (IRCRaised (NoMethodError _)) ->
                                                process_ m cmd' rest
                                            _ -> throwError ex)
                                case mstrs of
                                    [] -> ircPrivmsg towhere Nothing
                                    _  -> mapM_ (ircPrivmsg towhere . Just) mstrs)

                            (ircPrivmsg towhere . Just .((?name++" module failed: ")++)))

    --
    -- contextual messages are all input that isn't an explicit command.
    -- they're passed to all modules (todo, sounds inefficient) for
    -- scanning, and any that implement 'contextual' will reply.
    --
    -- we try to run the contextual functions from all modules, on every
    -- non-command. better hope this is efficient.
    --
    doContextualMsg r = do
        withAllModules (\m ->
            forkLB $ catchIrc
                (do ms <- contextual m msg alltargets r
                    mapM_ (ircPrivmsg alltargets . Just) ms)
                (\s -> debugStrLn
                    (?name++" module failed in contextual handler: "++s)) )
        return ()

------------------------------------------------------------------------

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text = case matchRegexAll re text of
      Nothing -> Nothing
      Just (_, _, cmd, _) -> Just cmd
    where re = mkRegex (nm ++ "[.:,]*[[:space:]]*")

-- | run an IO action in another thread, with a timeout, lifted into LB
forkLB :: LB a -> LB ()
forkLB f = (`liftLB` f) $ \g -> do
            forkIO $ do
                timeout (15 * 1000 * 1000) g
                return ()
            return ()

