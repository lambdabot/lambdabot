--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- Another progressive plugin. Compose two (for now) plugins transparently
-- A sort of mini interpreter. Could do with some more thinking.
--
module Plugins.Compose (theModule) where

import Util
import Lambdabot
import Data.List
import Control.Monad.State
import Control.Monad.Error
import GHC.IOBase   (Exception(NoMethodError))

newtype ComposeModule = ComposeModule ()

theModule :: MODULE
theModule = MODULE $ ComposeModule ()

instance Module ComposeModule () where
    moduleCmds _   = [".", "compose"]
    moduleHelp _ _ = ". <cmd1> <cmd2> [args].\n\ 
                     \. [or compose] is the composition of two plugins\n\ 
                     \ The following semantics are used: . f g xs == g xs >>= f"

    process    _ a b _ args = case split " " args of
        (f:g:xs) -> do
            f' <- lookupP (a,b) f
            g' <- lookupP (a,b) g
            compose f' g' (concat $ intersperse " " xs)

        _ -> return ["Not enough arguments to @."]


-- | Compose two plugin functions
compose :: (String -> LB [String]) -> (String -> LB [String]) -> (String -> LB [String])
compose f g xs = g xs >>= f . unlines

------------------------------------------------------------------------
-- | Lookup the `process' method we're after, and apply it to the dummy args
-- Fall back to process_ if there's no process.
--
lookupP :: (Message, String) -> String -> LB (String -> LB [String])
lookupP (a,b) cmd = withModule ircCommands cmd
    (error $ "Parse error: " ++ show cmd) 
    (\m -> do
        privs <- gets ircPrivCommands -- no priv commands can be composed
        when (cmd `elem` privs) $ error "Privledged commands cannot be composed"
        return $ \str -> catchError 
                    (process m a b cmd str)
                    (\ex -> case (ex :: IRCError) of 
                                (IRCRaised (NoMethodError _)) -> process_ m cmd str
                                _ -> throwError ex))

