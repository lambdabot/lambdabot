
module MoreModule (theModule) where

import IRC
import qualified Config (moresize, config)

newtype MoreModule = MoreModule ()

theModule :: MODULE
theModule = MODULE $ MoreModule ()

-- the @more state is handled centrally
instance Module MoreModule [String] where
    moduleHelp _ _ = return "@more - return more bot output"
    moduleCmds   _ = return ["more"]
    moduleInit   _ = ircInstallOutputFilter moreFilter
    process      _ _ target _ _ = do
        morestate <- readMS
        ircPrivmsg target $ unlines morestate

moreFilter :: String -> [String] -> ModuleT [String] IRC [String]
moreFilter _who msglines = do
  let maxLines = Config.moresize Config.config
      morelines = drop maxLines msglines
      thislines = take maxLines msglines
  writeMS $ morelines
  return thislines
