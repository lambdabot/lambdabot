--
-- | Look up sequences in the Online Encyclopedia of Integer Sequences
--   Based on the Math.OEIS library
--
module Plugin.OEIS (theModule) where

import Plugin

import Lib.OEIS
import Data.Char

PLUGIN OEIS

instance Module OEISModule () where
    moduleCmds   _     = ["oeis"]
    moduleHelp _ _     = "oeis <sequence>. Look up a sequence in the Online Encyclopedia of Integer Sequences"
    process _ _ to _ a = do s <- liftIO $ lookupOEIS a
                            out <- mapM (ios80 to) (map return s)
                            return $ concat out


lookupOEIS :: String -> IO [String]
lookupOEIS a = do
         let a'  = commas . reverse . dropWhile isSpace . reverse . dropWhile isSpace $ a
         x <- searchSequence_IO a'
         case x of
            Nothing -> do r <- random insult
                          return ["Sequence not found. " ++ r]
            Just s  -> return [description s, show $ sequenceData s]
 where commas []                     = []
       commas (x:' ':xs) | isDigit x = x : ',' : commas xs
       commas (x:xs)                 = x : commas xs
