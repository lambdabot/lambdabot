--
-- | Support for quotes
--
module Plugins.Slap (theModule) where

import Lambdabot
import qualified IRC
import Util                     (stdGetRandItem)

import Control.Monad.Trans      (liftIO)
import Control.Monad            (ap)

------------------------------------------------------------------------
newtype QuoteModule = QuoteModule ()

theModule :: MODULE
theModule = MODULE $ QuoteModule ()

instance Module QuoteModule () where
    moduleCmds           _ = ["slap"]
    moduleHelp _ "slap"    = "slap <nick>. Slap someone amusingly."

    process _ msg _ cmd rest = do
       quote <- liftIO $ case cmd of
                  "slap"  -> slapRandom (if rest == "me" then sender else rest)
                  _ -> error "QuoteModule: bad string"
       return [quote]
       where sender = IRC.nick msg

------------------------------------------------------------------------

-- | Return a random arr-quote
slapRandom :: String -> IO String
slapRandom x = ap (Util.stdGetRandItem slapList) (return x)

slapList :: [String -> String]
slapList = 
    [("/me slaps " ++)
    ,(\x -> "/me smacks " ++ x ++ " about with a large trout")
    ,("/me beats up " ++)
    ,("why on earth would I slap " ++)
    ]
