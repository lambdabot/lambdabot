--
-- | more support
--
module Plugins.More (theModule) where

import Lambdabot
import LBState
import qualified Config (moresize, config)

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE $ MoreModule ()

type MoreState = GlobalPrivate () [String]

-- the @more state is handled centrally
instance Module MoreModule MoreState where
    moduleHelp _ _ = return "@more - return more bot output"
    moduleCmds   _ = return ["more"]
    moduleDefState _ = return $ mkGlobalPrivate 20 ()
    moduleInit   _ = ircInstallOutputFilter moreFilter
    process      _ _ target _ _ = do
        morestate <- readPS target
        case morestate of
          Nothing -> ircPrivmsg' target "more: empty buffer"
          Just ls -> mapM_ (ircPrivmsg' target) =<< moreFilter target ls

moreFilter :: String -> [String] -> ModuleT MoreState LB [String]
moreFilter target msglines = do
  let maxLines = Config.moresize Config.config
      (morelines, thislines) = case drop (maxLines+2) msglines of
          [] -> ([],msglines)
          _  -> (drop maxLines msglines, take maxLines msglines)
  writePS target $ if null morelines then Nothing else Just morelines
  return $ thislines ++ if null morelines then [] 
    else ['[':shows (length morelines) " @more lines]"]
