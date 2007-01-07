module Tests where

import Control.Monad
import TestFramework

-- number of tests to run
n    = 10

run f = mapM_ (const f) [1..n]

$(tests "dummy" [d|

 testDummy = lb "dummy" "dummy"
 testEval  = lb "eval"  ""

 testId= run $ do
            s <- io80 random
            lb ("id " ++ s) (" " ++ s)

 testBug = lb "bug" "http://hackage.haskell.org/trac/ghc/newticket?type=bug"

 |])
