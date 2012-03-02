module UnitTestsMain where

import TestFramework
import Tests

allTests = TestList
    [ dummyPlugin
    , wherePlugin
    , sourcePlugin
    ]

main = runTestTT allTests
