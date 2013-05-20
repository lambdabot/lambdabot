-- | Look up sequences in the Online Encyclopedia of Integer Sequences
--   Based on the Math.OEIS library
module Lambdabot.Plugin.OEIS (oeisPlugin) where

import Lambdabot.Plugin

import Math.OEIS

oeisPlugin :: Module ()
oeisPlugin = newModule
    { moduleCmds = return
        [ (command "oeis")
            { aliases = ["sequence"]
            , help = say "oeis <sequence>. Look up a sequence in the Online Encyclopedia of Integer Sequences"
            , process = ios80 . fmap concat . lookupOEIS
            }
        ]
    }
