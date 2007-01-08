module Main where

import TestFramework
import Tests

allTests = TestList
    [ dummyPlugin
    , wherePlugin
    ]

main = runTestTT allTests
