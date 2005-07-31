--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Runplugs: use hs-plugins to run a Haskell expression under
-- controlled conditions.
--
import System.Eval.Haskell             (unsafeEval)

import Data.Maybe               (isJust, fromJust)
import Control.Monad            (when) 

import System.Exit              (exitWith, ExitCode(ExitSuccess))
import System.IO                (getContents, putStrLn)
import System.Posix.Resource    (setResourceLimit,
			         Resource(ResourceCPUTime), 
                                 ResourceLimits(ResourceLimits),
			         ResourceLimit(ResourceLimit))

rlimit = ResourceLimit 3

context = prehier ++ datas ++ qualifieds ++ controls

prehier = ["Char", "List", "Maybe", "Numeric", "Random" ]

qualifieds = ["qualified Data.Map as M", "qualified Data.Set as S"]

datas   = map ("Data." ++) [
                "Bits", "Bool", "Char", "Dynamic", "Either", 
                "Graph", "Int", "Ix", "List", 
                "Maybe", "Ratio", "Tree", "Tuple", "Typeable", "Word" 
              ]

controls = map ("Control." ++) ["Monad", "Monad.Reader", "Monad.Fix", "Arrow"]

main = do
        setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit)
        s <- getContents
        when (not . null $ s) $ do
                s <- unsafeEval ("(take 2048 (show ("++s++")))") context
                when (isJust s) (putStrLn (fromJust s))
        exitWith ExitSuccess

