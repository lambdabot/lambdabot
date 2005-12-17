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
    moduleHelp _ _ = "@more - return more bot output"
    moduleCmds   _ = ["more"]
    moduleDefState _ = return $ mkGlobalPrivate 20 ()
    moduleInit   _ = ircInstallOutputFilter moreFilter
    process      _ _ target _ _ = do
        morestate <- readPS target
        case morestate of
          Nothing -> return [] -- silently fail, it's ok.
          Just ls -> do mapM_ (ircPrivmsg' target) =<< moreFilter target ls
                        return []       -- special

moreFilter :: String -> [String] -> ModuleLB MoreState
moreFilter target msglines = do
  let maxLines = Config.moresize Config.config
      (morelines, thislines) = case drop (maxLines+2) msglines of
          [] -> ([],msglines)
          _  -> (drop maxLines msglines, take maxLines msglines)
  writePS target $ if null morelines then Nothing else Just morelines
  return $ thislines ++ if null morelines then [] 
    else ['[':shows (length morelines) " @more lines]"]
