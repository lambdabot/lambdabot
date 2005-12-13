--
-- Copyright (c) 2005 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
-- 
-- Written: Mon Dec 12 10:16:56 EST 2005
--

--
-- | A binding to Djinn.
--
module Plugins.Djinn (theModule) where

import Serial
import LBState
import Lambdabot      hiding  (clean)
import Util           hiding  (clean)
import PosixCompat

import Data.List                (intersperse)

import Control.Monad.Trans      (liftIO)
import Text.Regex

newtype DjinnModule = DjinnModule ()

theModule :: MODULE
theModule = MODULE $ DjinnModule ()

-- | We can accumulate an interesting environment
type DjinnEnv = [Decl]
type Decl = String

--
-- | Serialise environment.
-- Forkprocess djinn?
-- Load env on startup.. Or just load once per run? Probably cleaner.
-- Write env on shutdown.
--

instance Module DjinnModule DjinnEnv where

        moduleHelp _ s = return $ case s of
            "djinn"     -> "Generates Haskell code from a type.\n" ++
                           "http://darcs.augustsson.net/Darcs/Djinn"
            "djinn-add" -> "Define a new function type or type synonym"
            "djinn-del" -> "Remove a symbol from the environment"
            "djinn-env" -> "Show the current djinn environment"
            _           -> error "invalid command to Djinn.moduleHelp"

        moduleCmds      _ = return ["djinn","djinn-add","djinn-del","djinn-env"]
        moduleDefState  _ = return []
        moduleSerialize _ = Just listSerial

        -- rule out attempts to do IO, if these get into the env,
        -- they'll be executed by djinn
        process _ _ src _ s | Just _ <- cmd  `matchRegex` s = end
          where end  = ircPrivmsg src "Invalid command"
                cmd  = mkRegex "^ *:"

        -- Normal commands
        process _ _ src "djinn" s = do
                env  <- readMS
                o    <- liftIO $ djinn env $ ":set +sorted" <$> 
                                    "f ?" <+> s
                ircPrivmsg src o

        -- Augment environment
        process _ _ _ "djinn-add"  s = modifyMS $ \st -> (dropSpace s) : st

        -- Return the environment
        process _ _ src "djinn-env" _ = do 
            s <- readMS
            ircPrivmsg src (showClean . intersperse "\n" $ s)

        -- Remove sym from environment. We let djinn do the hard work
        -- Currently (Tue Dec 13 11:09:11 EST 2005) we can't remove type
        -- synonyms from the environment
        --
        process _ _ _ "djinn-del" s = do
            env  <- readMS
            env' <- liftIO $ djinn env $ ":delete" <+> dropSpace s <$> ":environment"
            modifyMS $ const . lines $ env'

        process _ _ _ _ _ = error "DjinnModule: invalid command"

------------------------------------------------------------------------

-- | Should be built inplace by the build system
binary :: String
binary = "./djinn"

-- | Call the binary:

djinn :: DjinnEnv -> String -> IO String
djinn env' src = do
    let env = concat . intersperse "\n" $ env'
    (out,err,_) <- popen binary [] (Just (env <$> src <$> ":q"))
    let o = dropNL . clean . unlines . init . drop 2 . lines $ out
        e = clean $ err
    return $ case () of {_
        | null o && null e -> "Terminated\n"
        | null o           -> e
        | otherwise        -> o
    }
    where
        dropNL = reverse . dropWhile (== '\n') . reverse

--
-- Clean up djinn output
--
clean :: String -> String
clean s | Just (a,_,b,_) <- prompt `matchRegexAll` s = a ++ clean b
        | Just (a,_,b,_) <- failed `matchRegexAll` s = a ++ clean b
        | otherwise      = s
    where
        prompt = mkRegex "Djinn>[^\n]*\n"
        failed = mkRegex "Cannot parse command[^\n]*\n"

