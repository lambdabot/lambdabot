{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | Look up sequences in the Online Encyclopedia of Integer Sequences
--   Based on the Math.OEIS library
module Plugin.OEIS (theModule) where

import Plugin

import Math.OEIS

$(plugin "OEIS")

instance Module OEISModule () where
    moduleCmds   _     = ["oeis"]
    moduleHelp _ _     = "oeis <sequence>. Look up a sequence in the Online Encyclopedia of Integer Sequences"
    process _ _ to _ a = do s <- liftIO $ lookupOEIS a
                            out <- mapM (ios80 to) (map return s)
                            return $ concat out
