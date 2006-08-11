--
-- | Persistent state
-- A demo plugin
--
module Plugin.State (theModule) where

import Plugin

PLUGIN State

instance Module StateModule String where
    moduleCmds      _ = ["state","snoc","cons"]
    moduleHelp    _ _ = "state [expr]. Get or set a state variable."
    moduleDefState  _ = return "This page intentionally left blank."
    moduleSerialize _ = Just stdSerial
    process_  _ _ []      = withMS $ \s _ -> return $ if null s then [] else [s]
    process_ _ "state"  t = withMS $ \_ w -> w t >> return [t]
    process_ _ "snoc"  t = withMS $ \s w -> w t >> return [s++t]
    process_ _ "cons"  t = withMS $ \s w -> w t >> return [t++s]
