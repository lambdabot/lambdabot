{-# LANGUAGE TemplateHaskell #-}
-- | Look up sequences in the Online Encyclopedia of Integer Sequences
--   Based on the Math.OEIS library
module Plugin.OEIS (theModule) where

import Plugin

import Math.OEIS

plugin "OEIS"

instance Module OEISModule where
    moduleCmds _ =
        [ (command "oeis")
            { aliases = ["sequence"]
            , help = say "oeis <sequence>. Look up a sequence in the Online Encyclopedia of Integer Sequences"
            , process = \a -> do
                to <- getTarget
                s <- io $ lookupOEIS a
                out <- mapM (ios80 to) (map return s)
                mapM_ say $ concat out
            }
        ]
