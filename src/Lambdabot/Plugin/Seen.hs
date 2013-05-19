-- Copyright (c) 2004 Thomas Jaeger
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Keep track of IRC users.
module Lambdabot.Plugin.Seen (theModule) where

import Lambdabot
import Lambdabot.Compat.AltTime
import Lambdabot.Compat.PackedNick
import Lambdabot.IRC
import qualified Lambdabot.Message as G
import Lambdabot.Nick
import Lambdabot.Plugin
import Lambdabot.Plugin.Seen.StopWatch
import Lambdabot.Plugin.Seen.UserStatus

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Binary
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.List
import qualified Data.Map as M
import System.IO
import Text.Printf

type SeenState = (MaxMap, SeenMap)
type SeenMap   = M.Map PackedNick UserStatus
type MaxMap    = M.Map Channel Int

type Seen = ModuleT SeenState LB

------------------------------------------------------------------------

theModule :: Module (M.Map Channel Int, M.Map PackedNick UserStatus)
theModule = newModule
    { moduleDefState = return (M.empty,M.empty)

    , moduleCmds = return
        [ (command "users")
            { help = say "users [chan]. Report the maximum number of users seen in a channel, and active users in the last 30 minutes"
            , process = doUsers
            }
        , (command "seen")
            { help = say "seen <user>. Report if a user has been seen by the bot"
            , process = doSeen
            }
        ]

    , moduleInit = do
        sequence_
            [ ircSignalConnect signal =<< bindModule1 (withSeenFM cb)
            | (signal, cb) <- zip
                ["JOIN", "PART", "QUIT", "NICK", "353",      "PRIVMSG"]
                [joinCB, partCB, quitCB, nickCB, joinChanCB, msgCB]
            ]

        c <- lb $ findOrCreateLBFile "seen"
        s <- io $ P.readFile c
        let ls = L.fromChunks [s]
        mbDecoded <- io . try . evaluate $ decode ls
        case mbDecoded of
            Left exc -> do
                -- try reading the old format (slightly different type... oh, "binary"...)
                mbOld <- io . try . evaluate $ decode ls
                case mbOld of
                    Left exc2 -> do
                        let _ = exc2 :: SomeException
                        io $ hPutStrLn stderr ("WARNING: failed to read Seen module state: "  ++ show (exc :: SomeException))
                    Right (maxMap, seenMap) ->
                        writeMS (M.mapKeys P.pack maxMap, seenMap)
            Right decoded -> writeMS decoded

    , moduleExit = do
        chans <- lift $ ircGetChannels
        unless (null chans) $ do
            ct    <- io getClockTime
            modifyMS $ \(n,m) -> (n, botPart ct (map packNick chans) m)

        -- and write out our state:
        withMS $ \s _ -> lb (findOrCreateLBFile "seen") >>= \ c -> io (encodeFile c s)
    }

lcNick :: Nick -> Nick
lcNick (Nick svr nck) = Nick svr (map toLower nck)

------------------------------------------------------------------------

doUsers :: String -> Cmd Seen ()
doUsers rest = withMsg $ \msg -> do
    -- first step towards tracking the maximum number of users
    chan <- getTarget
    (m, seenFM) <- readMS
    s <- io getClockTime
    let who = packNick $ lcNick $ if null rest then chan else parseNick (G.server msg) rest
        now = length [ () | (_,Present _ chans) <- M.toList seenFM
                          , who `elem` chans ]

        n = case M.lookup who m of Nothing -> 1; Just n' -> n'

        active = length [() | (_,st@(Present _ chans)) <- M.toList seenFM
                            , who `elem` chans && isActive st ]

        isActive (Present (Just (ct,_td)) _cs) = recent ct
        isActive _                             = False

        recent t = diffClockTimes s t < gap_minutes
        gap_minutes = TimeDiff 1800 -- 30 minutes

        percent p q = 100 * (fromIntegral p / fromIntegral q) :: Double

        total   0 0 = "0"
        total   p q = printf "%d (%0.1f%%)" p (percent p q)

    say $! printf "Maximum users seen in %s: %d, currently: %s, active: %s"
        (fmtNick (G.server msg) $ unpackNick who) n (total now n) (total active now)

doSeen :: String -> Cmd Seen ()
doSeen rest = withMsg $ \msg -> do
    target <- getTarget
    (_,seenFM) <- readMS
    now        <- io getClockTime
    let (txt,safe) = (getAnswer msg rest seenFM now)
    if safe || not ("#" `isPrefixOf` nName target)
        then mapM_ say txt
        else lb (ircPrivmsg (G.nick msg) (unlines txt))

getAnswer :: G.Message a => a -> String -> SeenMap -> ClockTime -> ([String], Bool)
getAnswer msg rest seenFM now
    | null nick' =
        let people  = map fst $ filter isActive $ M.toList seenFM
            isActive (_nick,state) = case state of
                (Present (Just (ct,_td)) _cs) -> recent ct
                _ -> False
            recent t = diffClockTimes now t < gap_minutes
            gap_minutes = TimeDiff 900 -- 15 minutes
         in (["Lately, I have seen " ++ (if null people then "nobody"
                 else listToStr "and" (map upAndShow people)) ++ "."], False)

    | pnick == G.lambdabotName msg =
        case M.lookup (packNick pnick) seenFM of
            Just (Present _ cs) ->
                (["Yes, I'm here. I'm in " ++ listToStr "and" (map upAndShow cs)], True)
            _ -> error "I'm here, but not here. And very confused!"

    | head (nName pnick) == '#' =
        let people  = map fst $ filter inChan $ M.toList seenFM
            inChan (_nick,state) = case state of
                (Present (Just _) cs)
                    -> packNick pnick `elem` cs
                _   -> False
         in (["In "++nick'++" I can see "
            ++ (if null people then "nobody"    -- todo, how far back does this go?
               else listToStr "and" (map upAndShow people)) ++ "."], False)

    | otherwise        = (return $ concat (case M.lookup (packNick pnick) seenFM of
        Just (Present mct cs)            -> nickPresent mct (map upAndShow cs)
        Just (NotPresent ct td chans)    -> nickNotPresent ct td (map upAndShow chans)
        Just (WasPresent ct sw _ chans)  -> nickWasPresent ct sw (map upAndShow chans)
        Just (NewNick newnick)           -> nickIsNew newnick
        _ -> ["I haven't seen ", nick, "."]), True)
    where
        -- I guess the only way out of this spagetty hell are printf-style responses.
        upAndShow = fmtNick (G.server msg) . unpackNick
        nickPresent mct cs =
            [ if you then "You are" else nick ++ " is"
            , " in ", listToStr "and" cs, "."
            , case mct of
                Nothing          -> concat [" I don't know when ", nick, " last spoke."]
                Just (ct,missed) -> prettyMissed (Stopped missed)
                       (concat [" I last heard ", nick, " speak ",
                                lastSpoke {-, ", but "-}])
                       (" Last spoke " ++ lastSpoke)
                    where lastSpoke = clockDifference ct
            ]
        nickNotPresent ct missed chans =
            [ "I saw ", nick, " leaving ", listToStr "and" chans, " "
            , clockDifference ct, prettyMissed missed ", and " ""
            ]
        nickWasPresent ct sw chans =
            [ "Last time I saw ", nick, " was when I left "
            , listToStr "and" chans , " ", clockDifference ct
            , prettyMissed sw ", and " ""
            ]
        nickIsNew newnick =
            [ if you then "You have" else nick++" has"
            , " changed nick to ", us, "."
            ] ++ fst (getAnswer msg us seenFM now)
            where
                us = upAndShow $ findFunc newnick
                findFunc pstr = case M.lookup pstr seenFM of
                    Just (NewNick pstr') -> findFunc pstr'
                    Just _               -> pstr
                    Nothing              -> error "SeenModule.nickIsNew: Nothing"

        nick' = takeWhile (not . isSpace) rest
        you   = pnick == lcNick (G.nick msg)
        nick  = if you then "you" else nick'
        pnick = lcNick $ parseNick (G.server msg) nick'
        clockDifference past
            | all (==' ') diff = "just now"
            | otherwise        = diff ++ " ago"
            where diff = timeDiffPretty . diffClockTimes now $ past

        prettyMissed (Stopped _) _ifMissed _     = "." -- ifMissed ++ "."
        prettyMissed _           _ _ifNotMissed  = "." -- ifNotMissed ++ "."

{-
        prettyMissed (Stopped missed) ifMissed _
            | missedPretty <- timeDiffPretty missed
            , any (/=' ') missedPretty
            = concat [ifMissed, "I have missed ", missedPretty, " since then."]

        prettyMissed _ _ ifNotMissed = ifNotMissed ++ "."
-}

-- | extract channels from message as packed, lower cased, strings.
msgChans :: G.Message a => a -> [Channel]
msgChans = map (packNick . lcNick) . G.channels

-- | Callback for when somebody joins. If it is not the bot that joins, record
--   that we have a new user in our state tree and that we have never seen the
--   user speaking.
joinCB :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
joinCB msg _ct nick fm
    | nick == lbNick = Right fm
    | otherwise      = Right $! insertUpd (updateJ Nothing chans) nick newInfo fm
    where
        insertUpd f = M.insertWith (\_ -> f)
        lbNick  = packNick $ G.lambdabotName msg
        newInfo = Present Nothing chans
        chans   = msgChans msg

-- | Update the state to reflect the bot leaving channel(s)
botPart :: ClockTime -> [Channel] -> SeenMap -> SeenMap
botPart ct cs = fmap botPart'
    where
        botPart' (Present mct xs) = case xs \\ cs of
            [] -> WasPresent ct (startWatch ct zeroWatch) mct cs
            ys -> Present mct ys
        botPart' (NotPresent ct' missed c)
            | head c `elem` cs = NotPresent ct' (startWatch ct missed) c
        botPart' (WasPresent ct' missed mct c)
            | head c `elem` cs = WasPresent ct' (startWatch ct missed) mct c
        botPart' us = us

-- | when somebody parts
partCB :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
partCB msg ct nick fm
    | nick == lbNick = Right $ botPart ct (msgChans msg) fm
    | otherwise      = case M.lookup nick fm of
        Just (Present mct xs) ->
            case xs \\ (msgChans msg) of
                [] -> Right $! M.insert nick (NotPresent ct zeroWatch xs) fm
                ys -> Right $! M.insert nick (Present mct ys)             fm
        _ -> Left "someone who isn't known parted"
    where lbNick = packNick $ G.lambdabotName msg

-- | when somebody quits
quitCB :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
quitCB _ ct nick fm = case M.lookup nick fm of
    Just (Present _ct xs) -> Right $! M.insert nick (NotPresent ct zeroWatch xs) fm
    _                     -> Left "someone who isn't known has quit"

-- | when somebody changes his\/her name
nickCB :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
nickCB msg _ nick fm = case M.lookup nick fm of
    Just status -> Right $! M.insert lcnewnick status
                          $ M.insert nick (NewNick lcnewnick) fm
    _           -> Left "someone who isn't here changed nick"
    where
        newnick = drop 1 $ head (ircMsgParams msg)
        lcnewnick = packNick $ lcNick $ parseNick (G.server msg) newnick

-- | when the bot joins a channel
joinChanCB :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
joinChanCB msg now _nick fm
    = Right $! fmap (updateNP now chan) (foldl insertNick fm chanUsers)
    where
        l = ircMsgParams msg
        chan = packNick $ lcNick $ parseNick (G.server msg) $ l !! 2
        chanUsers = map (packNick . lcNick . parseNick (G.server msg)) $ words (drop 1 (l !! 3)) -- remove ':'
        unUserMode nick = Nick (nTag nick) (dropWhile (`elem` "@+") $ nName nick)
        insertUpd f = M.insertWith (\_ -> f)
        insertNick fm' u = insertUpd (updateJ (Just now) [chan])
            (packNick . unUserMode . lcNick . unpackNick $ u)
            (Present Nothing [chan]) fm'

-- | when somebody speaks, update their clocktime
msgCB :: IrcMessage -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap
msgCB _ ct nick fm =
    case M.lookup nick fm of
        Just (Present _ xs) -> Right $!
            M.insert nick (Present (Just (ct, noTimeDiff)) xs) fm
        _ -> Left "someone who isn't here msg us"


-- | Callbacks are only allowed to use a limited knowledge of the world.
-- 'withSeenFM' is (up to trivial isomorphism) a monad morphism from the
-- restricted
--   'ReaderT (IRC.Message, ClockTime, Nick) (StateT SeenState (Error String))'
-- to the
--   'ReaderT IRC.Message (Seen IRC)'
-- monad.
withSeenFM :: G.Message a
           => (a -> ClockTime -> PackedNick -> SeenMap -> Either String SeenMap)
           -> (a -> Seen ())

withSeenFM f msg = do
    let chan = packNick . lcNick . head . G.channels $! msg
        nick = packNick . lcNick . G.nick $ msg

    withMS $ \(maxUsers,state) writer -> do
        ct <- io getClockTime
        case f msg ct nick state of
            Left _         -> return ()
            Right newstate -> do
                let curUsers = length $!
                        [ () | (_,Present _ chans) <- M.toList state
                        , chan `elem` chans ]

                    newMax = case M.lookup chan maxUsers of
                        Nothing -> M.insert chan curUsers maxUsers
                        Just  n -> if n < curUsers
                            then M.insert chan curUsers maxUsers
                            else maxUsers

                newMax `seq` newstate `seq` writer (newMax, newstate)
