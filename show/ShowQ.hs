{-# LANGUAGE NoMonomorphismRestriction #-}
-- Helper code for runplugs

module ShowQ where

import Data.List (group, intercalate, sort)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (numTests, quickCheckWithResult, stdArgs, Result(..), Testable)
import qualified Test.SmallCheck as SC (smallCheck, Testable)

mysmallcheck :: (SC.Testable prop) => prop -> ()
mysmallcheck = unsafePerformIO . mysmallcheck'
mysmallcheck' :: (SC.Testable prop) => prop -> IO ()
mysmallcheck' = SC.smallCheck 6

myquickcheck :: Testable prop => prop -> String
myquickcheck = unsafePerformIO . myquickcheck'

myquickcheck' :: Testable prop => prop -> IO String
myquickcheck' a = tests a 0 []

tests :: (Testable prop) => prop -> Int -> [[String]] -> IO String
tests prop ntest stamps =
  do result <- quickCheckWithResult stdArgs prop
     case result of
       NoExpectedFailure _ _ _ -> done "Arguments exhausted after" (numTests result) stamps
       GaveUp _ _ _ -> done "Arguments exhausted after" (numTests result) stamps
       Success _ _ _  -> done "OK, passed" (numTests result) stamps
       Failure _ _ _ _ _ _ _ -> return $ "Falsifiable, after "
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

