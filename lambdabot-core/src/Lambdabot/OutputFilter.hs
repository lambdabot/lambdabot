module Lambdabot.OutputFilter
    ( OutputFilter
    , textwidth
    
    , cleanOutput
    , lineify
    , checkRecip
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
        msg' = map dropSpaceEnd msg

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

-- | Don't send any output to alleged bots.
checkRecip :: Monad m => OutputFilter m
checkRecip who msg
--  TODO: this doesn't work with plugin protocols :(
--  | who == Config.name Config.config                  = return []
    | "bot" `isSuffixOf` map toLower (nName who)    = return []
    | otherwise                                         = return msg

-- | Divide the lines' indent by two
{-
reduceIndent :: OutputFilter
reduceIndent _ msg = return $ map redLine msg
    where
        redLine (' ':' ':xs)        = ' ':redLine xs
        redLine xs                  = xs
-}
