{-# LANGUAGE PatternGuards #-}

module Main where

-- A time- and output-limited unlambda

import System.Posix.Resource
import Data.Char
import System.IO

import Language.Unlambda

main :: IO Exp
main = (setResourceLimit ResourceCPUTime $ ResourceLimits (ResourceLimit 5) (ResourceLimit 5)) >> run

run :: IO Exp
run = do
  expr <- parse stdin
  let (Eval cp) = eval expr
  cp (Nothing, 2048) (const return)

------------------------------------------------------------------------
-- Parsing of the Unlambda program directly from handle
--
parse :: Handle -> IO Exp
parse h = do
  c <- catch (hGetChar h) (\_ -> error "Parse error at end of file")
  case toLower c of
    d | d `elem` " \t\n"  -> parse h
    '`' -> do e1 <- parse h
              e2 <- parse h
              return (App e1 e2)
    '#' -> hGetLine h >> parse h
    '.' -> hGetChar h >>= return . Dot
    '?' -> hGetChar h >>= return . Ques
    _ | Just fn <- lookup c table -> return fn
      | otherwise                 -> error $ "Unknown operator " ++ show c

    where table = zip "ksivcdre@|" [K,S,I,V,C,D,Dot '\n',E,At,Pipe]
