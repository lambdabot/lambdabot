module Lambdabot.OutputFilter
    ( OutputFilter
    , textwidth
    
    , cleanOutput
    , lineify
    ) where

import Lambdabot.Nick
import Lambdabot.Util

import Data.Char
import Data.List

type OutputFilter m = Nick -> [String] -> m [String]

textwidth :: Int
textwidth = 200 -- IRC maximum msg length, minus a bit for safety.

-- | For now, this just checks for duplicate empty lines.
cleanOutput :: Monad m => OutputFilter m
cleanOutput _ msg = return $ remDups True msg'
    where
        remDups True  ([]:xs) =    remDups True xs
        remDups False ([]:xs) = []:remDups True xs
        remDups _     (x: xs) = x: remDups False xs
        remDups _     []      = []
        msg' = map (dropFromEnd isSpace) msg

-- | wrap long lines.
lineify :: Monad m => OutputFilter m
lineify = const (return . mlines . unlines)

-- | break into lines
mlines :: String -> [String]
mlines = (mbreak =<<) . lines
    where
        mbreak :: String -> [String]
        mbreak xs
            | null bs   = [as]
            | otherwise = (as++cs) : filter (not . null) (mbreak ds)
            where
                (as,bs) = splitAt (w-n) xs
                breaks  = filter (not . isAlphaNum . last . fst) $ drop 1 $
                                  take n $ zip (inits bs) (tails bs)
                (cs,ds) = last $ (take n bs, drop n bs): breaks
                w = textwidth
                n = 10
