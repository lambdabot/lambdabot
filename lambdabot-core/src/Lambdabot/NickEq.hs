--
-- | Nickname equality subsystem.
--
-- This component is responsible for deciding whether two nicknames
-- refer to the same person, for the purposes of @tell et al.  Nickname
-- equality must be monadic because it uses mutable state maintained
-- by the @link and @unlink commands.
--
-- Also provided is a concept of polynicks (by analogy to polytypes);
-- polynicks can refer to an (open) set of nicknames.  For instance '@tell
-- *lambdabot Why does X do Y' could tell a message to anyone who has
-- identified as a lambdabot maintainer.  A polynick consists of a
-- bar-separated list of (nicks or open terms); an open term is like a
-- nick but preceded with a star.

module Lambdabot.NickEq
    ( Polynick
    , nickMatches
    , readPolynick
    , showPolynick
    
    , lookupMononickMap
    , mononickToPolynick
    ) where

import Lambdabot.Message
import Lambdabot.Monad
import Lambdabot.Nick

import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Polynick = Polynick [Nick] deriving (Eq) -- for now

-- |Determine if a nick matches a polynick.  The state is read at the
-- point of binding.
nickMatches :: LB (Nick -> Polynick -> Bool)
nickMatches = return m'
    where
      m' nck (Polynick nck2) = nck `elem` nck2

-- | Parse a read polynick.
readPolynick :: Message a => a -> String -> Polynick
readPolynick m = Polynick . map (parseNick (server m)) . splitOn "|"

-- | Format a polynick.
showPolynick :: Message a => a -> Polynick -> String
showPolynick m (Polynick n) = intercalate "|" $ map (fmtNick (server m)) n

-- | Convert a regular mononick into a polynick.
mononickToPolynick :: Nick -> Polynick
mononickToPolynick = Polynick . (:[])

-- | Lookup (using a polynick) in a map keyed on mononicks.
lookupMononickMap :: LB (Polynick -> M.Map Nick a -> [(Nick,a)])
lookupMononickMap = return $ look'
    where look' (Polynick ns) m = mapMaybe (\n -> (,) n `fmap` M.lookup n m) ns
