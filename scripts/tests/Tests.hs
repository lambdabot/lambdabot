module Tests where

import Control.Monad
import TestFramework

n    = 10

run f = mapM_ (const f) [1..n]

$(tests "dummy" [d|

 test1 = lb "dummy" "dummy"

 test2 = run $ do
            s <- io80 random
            lb (" " ++ s) ("id " ++ s)

 |])
