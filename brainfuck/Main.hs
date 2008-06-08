module Main where

import Language.Brainfuck
import Language.Brainfuck.Examples
import Control.Monad (when)

import Data.Array hiding (array)
import System.Posix.Resource

main :: IO ()
main = do
  setResourceLimit ResourceCPUTime $ ResourceLimits (ResourceLimit 5) (ResourceLimit 5)
  run

run = do
  prog <- getContents
  c    <- core
  let cmds = loadProgram prog
  when debug $ print cmds
  execute cmds (snd (bounds cmds)) (BF c 0 0)
