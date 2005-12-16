--
-- | Hackish Haddock module.
--
module Plugins.Haddock (theModule) where

import Lambdabot
import LBState
import Serial
import Util (concatWith)
import qualified Map as M

import qualified Data.FastPackedString as P

newtype HaddockModule = HaddockModule ()

theModule :: MODULE
theModule = MODULE $ HaddockModule ()

type HaddockState = M.Map P.FastString [P.FastString]

instance Module HaddockModule HaddockState where
    moduleCmds      _ = ["index"]
    moduleDefState  _ = return M.empty
    moduleSerialize _ = Just $ Serial {
              deserialize = Just . readPacked,
              serialize   = const Nothing }
    process      _ _ target _ rest = do
       assocs <- readMS
       ircPrivmsg target $ maybe "bzzt" (Util.concatWith ", " . (map P.unpack))
		                        (M.lookup (P.pack (stripParens rest)) assocs)

-- | make \@index ($) work.
stripParens :: String -> String
stripParens = reverse . dropWhile (==')') . reverse . dropWhile (=='(')

