module Tests where

import Control.Monad
import TestFramework

-- number of tests to run
n    = 10

run f = mapM_ (const f) [1..n]

------------------------------------------------------------------------
--
-- Test the dummy plugin
--
$(tests "dummyPlugin" [d|

 testDummy = lb "dummy" "dummy"
 testEval  = lb "eval"  ""

 testId= run $ do
            s <- io80 random
            lb ("id " ++ s) (" " ++ s)

 testBug = lb "bug" "http://hackage.haskell.org/trac/ghc/newticket?type=bug"

 |])

------------------------------------------------------------------------
--
-- Test the Where plugin
--

$(tests "wherePlugin" [d|
    testWhere   = lb "where ghc" "http://haskell.org/ghc"
    testWhat    = lb "where ghc" "http://haskell.org/ghc"
    testUrl     = lb "where ghc" "http://haskell.org/ghc"
 |] )

------------------------------------------------------------------------
--
-- Test the Source plugin
--

$(tests "sourcePlugin" [d|
    testSource  = lb "source foldr" $ unlines
        [ "foldr k z xs = go xs"
        ,"    where go []     = z"
        ,"          go (y:ys) = y `k` go ys"]

 |] )
