module Main where

import TestFramework
import Tests

allTests = TestList
    [ dummyPlugin
    ]

main = runTestTT allTests
