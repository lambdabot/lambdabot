--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Runplugs: use hs-plugins to run a Haskell expression under
-- controlled conditions.
--
import System.Eval.Haskell      (unsafeEval_)

import Data.Char                (chr)
import Data.Maybe               (isJust, fromJust)
import Control.Monad

import System.Random
import System.Exit              (exitWith, ExitCode(ExitSuccess))
import System.IO                (getContents, putStrLn)
import System.Posix.Resource    (setResourceLimit,
                                 Resource(ResourceCPUTime),
                                 ResourceLimits(ResourceLimits),
                                 ResourceLimit(ResourceLimit))

import qualified Control.Exception

rlimit = ResourceLimit 3

context = prelude ++ prehier ++ datas ++ qualifieds ++ controls ++ other
       ++ template ++ extras ++ numbers

prelude =
    ["qualified Prelude as P", "Prelude"]

other   =
    ["Text.Printf"
    ,"Text.PrettyPrint.HughesPJ hiding (empty)"
    ,"Math.OEIS"]

prehier =
    ["Numeric"]

qualifieds =
    ["qualified Data.Map                    as M"
    ,"qualified Data.IntMap                 as I"
    ,"qualified Data.ByteString             as S"
    ,"qualified Data.ByteString.Char8       as SC"
    ,"qualified Data.ByteString.Lazy        as L"
    ,"qualified Data.ByteString.Lazy.Char8  as LC"
    ,"qualified Data.Set"
    ,"qualified Data.Generics"
    ,"qualified Data.IntSet"
    ,"qualified Data.Foldable"
    ,"qualified Data.Sequence"
    ,"qualified Data.Traversable"
    ,"qualified Control.Monad.Writer"
    ]

datas   = map ("Data." ++)
    ["Array"
    ,"Bits"
    ,"Bool"
    ,"Char"
    ,"Complex"
    ,"Dynamic"
    ,"Either"
    ,"Eq"
    ,"Fixed"
--  ,"Foldable"
--  ,"Function"
--  ,"Generics"
    ,"Graph"
    ,"Int"
--  ,"IntMap"
--  ,"IntSet"
    ,"Ix"
    ,"List"
--  ,"Map"
    ,"Maybe"
    ,"Monoid"
    ,"Ord"
    ,"Ratio"
--  ,"Set"
    ,"Tree"
    ,"Tuple"
    ,"Typeable"
    ,"Word"
    ]

numbers = map ("Data.Number." ++)
    ["Symbolic"
    ,"Dif"
    ,"CReal"
    ,"Fixed"
    ,"Interval"
    ,"BigFloat"
    ,"Natural"]

controls = map ("Control." ++)
    ["Monad"
    ,"Monad.Cont"
    ,"Monad.Error"
    ,"Monad.Identity"
    ,"Monad.List"
    ,"Monad.RWS"
    ,"Monad.Reader"
    ,"Monad.State"
    ,"Monad.Trans"
    ,"Monad.Fix"
    ,"Monad.Instances"
    ,"Applicative"
    ,"Arrow hiding (pure)"
--  ,"Arrow.Transformer"
--  ,"Arrow.Transformer.All"
--  ,"Arrow.Operations"
    ,"Parallel"
    ,"Parallel.Strategies"
    ]

--
-- See if TH is safe with runIO and friends hidden.
--
-- Be very careful here. The semantics of what is safe and unsafe is a
-- bit blurry. It depends on what GHC allows.
--
template = [] -- ["Language.Haskell.TH hiding (runIO,reify)"]
extras   = ["ShowQ","ShowFun","L","LargeWord","SimpleReflect hiding (var)"]

main = do
    setResourceLimit ResourceCPUTime (ResourceLimits rlimit rlimit)
    s <- getLine
    when (not . null $ s) $ do
        x <- sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
        s <- unsafeEval_ ("let { "++x++
                         " = \n# 1 \"<irc>\"\n"++s++
                         "\n} in P.take 2048 (P.show "++x++
                         ")") context ["-O","-fasm","-fextended-default-rules"] [] []
        case s of
            Left  e -> mapM_ putStrLn e
            Right v -> Control.Exception.catch
                (putStrLn v)
                (\e -> Control.Exception.handle (const $ putStrLn "Exception") $ do
                            e' <- Control.Exception.evaluate e
                            putStrLn $ "Exception: " ++ take 1024 (show e'))
    exitWith ExitSuccess

