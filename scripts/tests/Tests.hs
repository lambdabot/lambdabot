module Tests where

import TestFramework

$(tests "dummyPlugin" [d|

 test1 = lb "dummy" "dummy"

 |])
