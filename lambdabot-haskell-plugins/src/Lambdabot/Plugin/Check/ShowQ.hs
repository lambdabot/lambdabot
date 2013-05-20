-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
-- Copyright date and holder unknown.
module Lambdabot.Plugin.Check.ShowQ (myquickcheck) where

import Data.List (group, intercalate, sort)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (numTests, quickCheckWithResult, stdArgs, Result(..), Testable)

myquickcheck :: Testable prop => prop -> String
myquickcheck = unsafePerformIO . myquickcheck'

myquickcheck' :: Testable prop => prop -> IO String
myquickcheck' a = tests a 0 []

tests :: (Testable prop) => prop -> Int -> [[String]] -> IO String
tests prop ntest stamps =
  do result <- quickCheckWithResult stdArgs prop
     case result of
       NoExpectedFailure{}  -> done "Arguments exhausted after" (numTests result) stamps
       GaveUp{}             -> done "Arguments exhausted after" (numTests result) stamps
       Success{}            -> done "OK, passed" (numTests result) stamps
       Failure{}            -> return $ "Falsifiable, after "
                                  ++ show ntest
                                  ++ " tests:\n"
                                  ++ reason result

done :: String -> Int -> [[String]] -> IO String
done mesg ntest stamps = return $ mesg ++ " " ++ show ntest ++ " tests" ++ table
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = "."
  display [x] = " (" ++ x ++ ")."
  display xs  = '.' : unlines (map (++ ".") xs)

  pairLength :: [a] -> (Int, a)
  pairLength [] = (0, error "pairLength should never get an empty list")
  pairLength xss@(xs:_) = (length xss, xs)

  entry (n, xs)         = percentage n ntest
                       ++ intercalate ", " xs

  percentage n m        = show ((100 * n) `div` m) ++ "%"

