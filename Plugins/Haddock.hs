--
-- | Hackish Haddock module.
--
module Plugins.Haddock (theModule) where

import Lambdabot
import LBState
import Util (Serializer(..), join, readM)
import qualified Map as M
import Data.Maybe (mapMaybe)
import Data.List  (foldl')

newtype HaddockModule = HaddockModule ()

theModule :: MODULE
theModule = MODULE $ HaddockModule ()

type HaddockState = M.Map String [String]

instance Module HaddockModule HaddockState where
    moduleHelp    _ _ = return "@index - yay!"
    moduleCmds      _ = return ["index"]
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just $ Serializer { 
      deSerialize = Just . readHaddockMap,
      serialize = const Nothing }
    process      _ _ target _ rest = do
       assocs <- readMS
       ircPrivmsg target $ maybe "bzzt" (Util.join ", ") (M.lookup (stripParens rest) assocs)

-- | This sucks, but we've got to do something about memory consumption.
--   I would have expected a greater effect though, after all there are
--   only 335 modules.
readHaddockMap :: String -> HaddockState
readHaddockMap str = fst $ foldl' step (M.empty,M.empty) assocs where
  assocs = mapMaybe readM $ lines str
  step :: (M.Map String [String], M.Map String String) -> (String, [String]) 
       -> (M.Map String [String], M.Map String String)
  step (amap, cmap) (key,value) = (amap', cmap') where
    amap' = M.insert key (map (cmap' M.!) value) amap
    cmap' = foldl' step' cmap value
    step' m v = case M.lookup v m of
      Nothing -> M.insert v v m
      Just _  -> m

-- | make @index ($) work.
stripParens :: String -> String
stripParens = reverse . dropWhile (==')') . reverse . dropWhile (=='(')

