{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | Support for quotes
module Plugin.Slap (theModule) where

import Plugin
import qualified Message (nick, showNick)

$(plugin "Quote")

instance Module QuoteModule () where
    moduleCmds _           = ["slap", "smack"]
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
    ,(\x -> "/me pokes " ++ x ++ " in the eye")
    ,(\x -> "why on earth would I slap " ++ x ++ "?")
    ,(\x -> "*SMACK*, *SLAM*, take that " ++ x ++ "!")
    ,(\_ -> "/me activates her slap-o-matic...")
    ,(\x -> "/me orders her trained monkeys to punch " ++ x)
    ,(\x -> "/me smashes a lamp on " ++ possesiveForm x ++ " head")
    ,(\x -> "/me hits " ++ x ++ " with a hammer, so they breaks into a thousand pieces")
    ,(\x -> "/me throws some pointy lambdas at " ++ x)
    ,(\x -> "/me loves " ++ x ++ ", so no slapping")
    ,(\x -> "/me would never hurt " ++ x ++ "!")
    ,(\x -> "go slap " ++ x ++ " yourself")
    ,(\_ -> "I won't; I want to go get some cookies instead.")
    ,(\x -> "I'd rather not; " ++ x ++ " looks rather dangerous.")
    ,(\_ -> "I don't perform such side effects on command!")
    ,(\_ -> "stop telling me what to do")
    ,(\x -> "/me clobbers " ++ x ++ " with an untyped language")
    ,(\x -> "/me pulls " ++ x ++ " through the Evil Mangler")
    ,(\x -> "/me secretly deletes " ++ possesiveForm x ++ " source code")
    ,(\x -> "/me places her fist firmly on " ++ possesiveForm x ++ " jaw")
    ,(\x -> "/me locks up " ++ x ++ " in a Monad")
    ,(\x -> "/me submits " ++ possesiveForm x ++ " email address to a dozen spam lists")
    ,(\x -> "/me moulds " ++ x ++ " into a delicous cookie, and places it in her oven")
    ,(\_ -> "/me will count to five...")
    ,(\x -> "/me jabs " ++ x ++ " with a C pointer")
    ,(\x -> "/me is overcome by a sudden desire to hurt " ++ x)
    ,(\x -> "/me karate-chops " ++ x ++ " into two equally sized halves")
    ,(\x -> "Come on, let's all slap " ++ x)
    ,(\x -> "/me pushes " ++ x ++ " from his chair")
    ,(\x -> "/me hits " ++ x ++ " with an assortment of kitchen utensils")
    ,(\x -> "/me slaps " ++ x ++ " with a slab of concrete")
    ,(\x -> "/me puts on her slapping gloves, and slaps " ++ x)
    ,(\x -> "/me decomposes " ++ x ++ " into several parts using the Banach-Tarski theorem and reassembles them to get two copies of " ++ x ++ "!")
    ]

-- | The possesive form of a name, "x's"
possesiveForm :: String -> String
possesiveForm [] = []
possesiveForm x
 | last x == 's' = x ++ "'"
 | otherwise     = x ++ "'s"
