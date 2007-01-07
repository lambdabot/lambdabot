module Main where

import TestFramework
import Tests

allTests = TestList
    [ dummy
    ]

main = runTestTT allTests
