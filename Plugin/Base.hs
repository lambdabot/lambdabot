{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
-- | Lambdabot base module. Controls message send and receive
module Plugin.Base (theModule) where

import Plugin

import IRCBase (IrcMessage, timeReply, errShowMsg)
-- import Message (getTopic, nick, joinChannel, body, fullName, channels)
import Message (getTopic, nick, server, body, Nick(..), lambdabotName, showNick, readNick)

import qualified Data.Map as M   (insert, delete)

import Control.Monad.State  (MonadState(..), when, gets)

import Control.OldException (Exception(NoMethodError))

import qualified Data.ByteString.Char8 as P
import qualified Text.Regex as R

-- valid command prefixes
commands :: [String]
commands  = commandPrefixes config

$(plugin "Base")

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
doJOIN msg | lambdabotName msg /= nick msg = doIGNORE msg
           | otherwise
  = do s <- get
       put (s { ircChannels = M.insert  (mkCN loc) "[currently unknown]" (ircChannels s)}) -- the empty topic causes problems
       send $ getTopic loc -- initialize topic
   where (_, aloc) = breakOnGlue ":" (head (body msg))
         loc       = case aloc of
                        [] -> Nick "freenode" "weird#"
                        _  -> Nick (server msg) (tail aloc)

doPART :: Callback
doPART msg
  = when (lambdabotName msg == nick msg) $ do
        let loc = Nick (server msg) (head (body msg))
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
    = do let loc = Nick (server msg) ((head (body msg)))
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ head $ tail $ body msg) (ircChannels s)})

doRPL_WELCOME :: Callback
doRPL_WELCOME = doIGNORE

doQUIT :: Callback
doQUIT msg = doIGNORE msg

doRPL_BOUNCE :: Callback
doRPL_BOUNCE _msg = debugStrLn "BOUNCE!"

doRPL_TOPIC :: Callback
doRPL_TOPIC msg -- nearly the same as doTOPIC but has our nick on the front of body
    = do let loc = Nick (server msg) ((body msg) !! 1)
         s <- get
         put (s { ircChannels = M.insert (mkCN loc) (tail $ last $ body msg) (ircChannels s) })

doPRIVMSG :: IrcMessage -> Base ()
doPRIVMSG msg = do
--  now <- io getClockTime
--  io $ appendFile (File.findFile "log") $ ppr now
    ignored <- lift $ checkIgnore msg
    if ignored
      then lift $ doIGNORE msg
      else mapM_ (doPRIVMSG' (lambdabotName msg) msg) targets
  where
    alltargets = head (body msg)
    targets = map (readNick msg) $ split "," alltargets
--  where
--    ppr now = concat [ timeStamp now, " ", "<", (nick msg), " ", (fullName msg), " #"
--                     , (concat . intersperse ","  $ channels msg) ,  "> "
--                     , (tail . concat . intersperse " " . tail) (body msg), "\n"]

--
-- | What does the bot respond to?
--
doPRIVMSG' :: Nick -> IrcMessage -> Nick -> Base ()
doPRIVMSG' myname msg target
  | myname == target
    = let (cmd, params) = breakOnGlue " " text
      in doPersonalMsg cmd (dropWhile (== ' ') params)

  | flip any ":," $ \c -> (showNick msg myname ++ [c]) `isPrefixOf` text
    = let Just wholeCmd = maybeCommand (showNick msg myname) text
          (cmd, params) = breakOnGlue " " wholeCmd
      in doPublicMsg cmd (dropWhile (==' ') params)

  | (commands `arePrefixesOf` text)
  && length text > 1
  && (text !! 1 /= ' ') -- elem of prefixes
  && (not (commands `arePrefixesOf` [text !! 1]) ||
      (length text > 2 && text !! 2 == ' ')) -- ignore @@ prefix, but not the @@ command itself
    = let (cmd, params) = breakOnGlue " " (dropWhile (==' ') text)
      in doPublicMsg cmd (dropWhile (==' ') params)

  | otherwise =  doContextualMsg text

  where
    text = tail (head (tail (body msg)))
    who = nick msg

    doPersonalMsg s r
        | commands `arePrefixesOf` s        = doMsg (tail s) r who
        | s `elem` (evalPrefixes config)    = doMsg "run"    r who
        | otherwise                         = (lift $ doIGNORE msg)

    doPublicMsg s r
        | commands `arePrefixesOf` s        = doMsg (tail s) r target
        | (evalPrefixes config) `arePrefixesWithSpaceOf` s = doMsg "run" r target -- TODO
        | otherwise                         = (lift $ doIGNORE msg)

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
                          -> lift . ircmsg $ "Maybe you meant: "++showClean(nub(ms++ss))
                  _ -> docmd cmd         -- no prefix, edit distance too far
        where
            e = 3   -- edit distance cut off. Seems reasonable for small words

            fcmd  = P.pack cmd      -- TODO
            frest = P.pack rest

            docmd cmd' = do
              act <- bindModule0 . withPS towhere $ \_ _ -> do
                withModule ircCommands cmd'   -- Important.
                    (ircPrivmsg towhere "Unknown command, try @list")
                    (\m -> do
                        name'   <- getName
                        privs   <- gets ircPrivCommands
                        let illegal = disabledCommands config
                        ok      <- liftM2 (||) (return $ cmd' `notElem` (privs ++ illegal))
                                               (lift $ checkPrivs msg)
                        if not ok
                          then lift $ ircPrivmsg towhere "Not enough privileges"
                          else catchIrc

                            (do mstrs <- catchError
                                 (Right `fmap` fprocess_ m fcmd frest)
                                 (\ex -> case (ex :: IRCError) of -- dispatch
                                   (IRCRaised (NoMethodError _)) -> catchError
                                        (Left `fmap` process m msg towhere cmd' rest)
                                        (\ey -> case (ey :: IRCError) of -- dispatch
                                            (IRCRaised (NoMethodError _)) ->
                                                Left `fmap` process_ m cmd' rest
                                            _ -> throwError ey)
                                   _ -> throwError ex)

                                -- send off our strings/bytestrings
                                case mstrs of
                                    Right ps -> lift $ mapM_ (ircPrivmsgF towhere) ps

                                    Left  ms -> lift $ mapM_ (ircPrivmsg towhere) ms)

                            (lift . ircPrivmsg towhere .
                                (("Plugin `" ++ name' ++ "' failed with: ") ++) . show))
              lift $ act
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
        withAllModules ( \m -> do
            act <- bindModule0 ( do
                            ms <- contextual m msg target r
                            lift $ mapM_ (ircPrivmsg target) ms
                   )
            name' <- getName
            lift $ catchIrc act (debugStrLn . (name' ++) .
                (" module failed in contextual handler: " ++) . show)
            )
        return ()

------------------------------------------------------------------------

maybeCommand :: String -> String -> Maybe String
maybeCommand nm text = case R.matchRegexAll re text of
      Nothing -> Nothing
      Just (_, _, cmd, _) -> Just cmd
    where re = regex' (nm ++ "[.:,]*[[:space:]]*")

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
