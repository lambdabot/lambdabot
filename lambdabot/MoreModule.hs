
module MoreModule (theModule) where

import IRC
import qualified Config (moresize, config)

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE $ MoreModule ()

type MoreState = GlobalPrivate () [String]

-- the @more state is handled centrally
instance Module MoreModule MoreState where
    moduleHelp _ _ = return "@more - return more bot output"
    moduleCmds   _ = return ["more"]
    moduleDefState _ = return $ mkGlobalPrivate ()
    moduleInit   _ = ircInstallOutputFilter moreFilter
    process      _ _ target _ _ = do
        morestate <- readPS target
        ircPrivmsg target $ maybe "more: empty buffer" unlines morestate

moreFilter :: String -> [String] -> ModuleT MoreState IRC [String]
moreFilter target msglines = do
  let maxLines = Config.moresize Config.config
      morelines = drop maxLines msglines
      thislines = take maxLines msglines
  writePS 15 target $ if null morelines then Nothing else Just morelines
  return $ thislines ++ if null morelines then [] 
    else ['[':shows (length morelines) " @more lines]"]
