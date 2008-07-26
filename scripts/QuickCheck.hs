-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | QuickCheck. use hs-plugins to run a Haskell expression under
-- controlled conditions.
import System.Eval.Haskell      (unsafeEval_)

import Data.Char                (chr)
import Data.List
import Control.Monad

import System.Random
import System.Exit              (exitWith, ExitCode(ExitSuccess))
import System.IO                (putStrLn)
import System.Posix.Resource

import Test.QuickCheck

import qualified Control.Exception

main :: IO ()
main = do
    setResourceLimit ResourceCPUTime $ ResourceLimits (ResourceLimit 5) (ResourceLimit 5)
    s <- getLine
    context <- fmap ((["L","ShowFun"]++)
                     . map (unwords . drop 1 . words)
                     . filter (isPrefixOf "import")
                     . lines)
                    (readFile "imports.h")
    when (not . null $ s) $ do
        x <- sequence (take 3 (repeat $ getStdRandom (randomR (97,122)) >>= return . chr))
        t <- unsafeEval_ ("let { "++x++
                         " = \n# 1 \"<irc>\"\n"++s++
                         "\n} in (myquickcheck' "++x++
                         ")") (context) ["-O","-XExtendedDefaultRules","-package oeis", "-XNoMonomorphismRestriction"] [] []
        case t of
            Left  e -> mapM_ putStrLn e
            Right a -> Control.Exception.catch
                (a >>= putStr . take 512)
                (\e -> Control.Exception.handle (const $ putStrLn "Exception") $ do
                            e' <- Control.Exception.evaluate e
                            putStrLn $ "Exception: " ++ take 128 (show e'))
    exitWith ExitSuccess

