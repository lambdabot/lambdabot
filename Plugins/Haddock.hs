--
-- | Hackish Haddock module.
--
module Plugins.Haddock (theModule) where

import IRC
import Util (mapSerializer, Serializer(..), join)
import qualified Map as M

newtype HaddockModule = HaddockModule ()

theModule :: MODULE
theModule = MODULE $ HaddockModule ()

type HaddockState = M.Map String [String]

instance Module HaddockModule HaddockState where
    moduleHelp    _ _ = return "@index - yay!"
    moduleCmds      _ = return ["index"]
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just $ mapSerializer { serialize = const Nothing }
    process      _ _ target _ rest = do
       assocs <- readMS
       ircPrivmsg target $ maybe "bzzt" (Util.join ", ") (M.lookup rest assocs)
