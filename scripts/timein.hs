#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import System.Cmd (system)

main :: IO ()
main = do (x:_) <- getArgs
          system $ "w3m -dump http://www.timeanddate.com/worldclock/results.html?query=" ++ x ++ " | grep 'zone offset'|cut --delimiter=' ' -f 10-"
          return ()