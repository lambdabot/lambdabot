-- | Look up sequences in the Online Encyclopedia of Integer Sequences
--   Based on the Math.OEIS library
module Lambdabot.Plugin.Reference.OEIS (oeisPlugin) where

import Lambdabot.Plugin

import Math.OEIS
import Data.Char

oeisPlugin :: Module ()
oeisPlugin = newModule
    { moduleCmds = return
        [ (command "oeis")
            { aliases = ["sequence"]
            , help = say "oeis <sequence>. Look up a sequence in the Online Encyclopedia of Integer Sequences"
            , process = ios80 . lookupOEIS'
            }
        ]
    }

lookupOEIS' :: String -> IO String
lookupOEIS' a = do
    let a' = commas . reverse . dropWhile isSpace . reverse . dropWhile isSpace $ a
    x <- searchSequence_IO a'
    case x of
        Nothing -> return "Sequence not found."
        Just s  -> return $ unlines [
            concat ("https://oeis.org/" : take 1 (catalogNums s)) ++
            ' ' : description s,
            show $ sequenceData s]
  where
    commas []                     = []
    commas (x:' ':xs) | isDigit x = x : ',' : commas xs
    commas (x:xs)                 = x : commas xs
