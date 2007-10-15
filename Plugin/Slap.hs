--
-- | Support for quotes
--
module Plugin.Slap (theModule) where

import Plugin
import qualified Message (nick, showNick)

PLUGIN Quote

instance Module QuoteModule () where
    moduleCmds _           = ["slap"]
    moduleHelp _ _         = "slap <nick>. Slap someone amusingly."
    process _ msg _ _ rest = ios $ slapRandom (if rest == "me" then sender else rest)
       where sender = Message.showNick msg $ Message.nick msg

------------------------------------------------------------------------

-- | Return a random arr-quote
slapRandom :: String -> IO String
slapRandom = (randomElem slapList `ap`) . return

slapList :: [String -> String]
slapList =
    [(\x -> "/me slaps " ++ x)
    ,(\x -> "/me smacks " ++ x ++ " about with a large trout")
    ,(\x -> "/me beats up " ++ x)
    ,(\x -> "why on earth would I slap " ++ x ++ "?")
    ,(\x -> "*SMACK*, *SLAM*, take that " ++ x ++ "!")
    ,(\_ -> "/me activates his slap-o-matic...")
    ,(\x -> "/me orders his trained monkeys to punch " ++ x)
    ,(\x -> "/me smashes a lamp on " ++ possesiveForm x ++ " head")
    ,(\x -> "/me hits " ++ x ++ " with a hammer, so he breaks into a thousand pieces")
    ,(\x -> "/me throws some pointy lambdas at " ++ x)
    ,(\x -> "/me loves " ++ x ++ ", so no slapping")
    ,(\x -> "/me would never hurt " ++ x ++ "!")
    ,(\x -> "go slap " ++ x ++ " yourself")
    ,(\x -> "/me clobbers " ++ x ++ " with an untyped language")
    ,(\x -> "/me pulls " ++ x ++ " through the Evil Mangler")
    ]

-- | The possesive form of a name, "x's"
possesiveForm :: String -> String
possesiveForm [] = []
possesiveForm x
 | last x == 's' = x ++ "'"
 | otherwise     = x ++ "'s"
