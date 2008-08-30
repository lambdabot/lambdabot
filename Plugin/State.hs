{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Persistent state
-- A demo plugin
module Plugin.State (theModule) where

import Plugin

$(plugin "State")

instance Module StateModule String where
    moduleCmds      _ = [] -- ["state","++"]
    moduleHelp    _ _ = "state [expr]. Get or set a state variable."
    moduleDefState  _ = return "This page intentionally left blank."
    moduleSerialize _ = Just stdSerial
    process_  _ _ []      = withMS $ \s _ -> return $ if null s then [] else [s]
    process_ _ "state" t  = withMS $ \_ w -> w t >> return [t]
    process_ _ "++"    t  = withMS $ \s w -> w s >> return [s++t]

{-

An example of scripting lambdabot. Implementing Rock/paper/scissors:

20:07  dons:: ?state (\xs -> case last (init xs) of '1' -> "rock"; '2' -> "paper"; '3' -> "scissors")
20:07  lambdabot:: (\xs -> case last (init xs) of '1' -> "rock"; '2' -> "paper"; '3' -> "scissors")

20:08  dons:: now, compose another plugin, with this state

20:08  dons:: ?. read . run . ++ . show dice 1d3
20:08  lambdabot::  paper
20:08  dons:: ?. read . run . ++ . show dice 1d3
20:08  lambdabot::  rock

-}
