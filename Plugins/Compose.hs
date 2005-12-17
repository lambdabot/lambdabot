{-# OPTIONS -w #-}
--
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- Another progressive plugin. Compose two (for now) plugins transparently
--
module Plugins.Compose (theModule) where

import Util
import Lambdabot
import Data.List
import Control.Monad
import Control.Monad.State

newtype ComposeModule = ComposeModule ()

theModule :: MODULE
theModule = MODULE $ ComposeModule ()

instance Module ComposeModule () where
    moduleCmds _   = ["."]
    moduleHelp _ _ = "@. cmd2 cmd1 arg == cmd2 . cmd1 $ arg"
    process    _ a b _ args =
        let (f,as) = break (==' ') args -- no error checking...
            (g,bs) = break (==' ') tail as
            xs     = tail bs
        in doCompose (a,b) f g xs

doCompose :: (Message, String) -> String -> String -> String -> LB [String]
doCompose ms f g xs = do
    f' <- lookupP ms f
    g' <- lookupP ms g
    f' xs >>= g' . concat

-- | Lookup the `process' method we're after, and apply it to the dummy args
lookupP :: (Message, String) -> String -> LB (String -> LB [String])
lookupP (a,b) cmd = withModule ircCommands cmd
    (error $ "No such command: " ++ show cmd)
    (\m -> do
        privs <- gets ircPrivCommands -- no priv commands can be composed
        when (cmd `elem` privs) $ error "Privledged commands cannot be composed"
        return $ process m a b cmd)

