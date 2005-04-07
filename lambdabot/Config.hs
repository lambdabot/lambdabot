
module Config where

data Config = Config {
        name      :: String,
        userinfo  :: String,
        host      :: String,
        port      :: Int,
        verbose   :: Bool,
        moresize  :: Int,
        autojoin  :: [String],
        admins    :: [String],
        proxy     :: Maybe ([Char], Integer)
}

--
-- Useful defaults for #haskell.
--
config :: Config
config = Config {
        name            = "lambdabot",
        userinfo        = "Lambda Robots - 100% Loyal",
        host            = "irc.eu.freenode.net",

        port            = 6667,
        verbose         = True,
        moresize        = 7,
        proxy           = Nothing,
        autojoin        = ["#haskell"],

        admins          = [
                "Pseudonym",    "shapr",        "pesco",        "Riastradh",
                "Darius",       "tmoertel",     "delYsid",      "polli",
                "Heffalump",    "Igloo",        "Marvin--",     "o3",
                "phubuh",       "ddarius",      "bringert",     "dons",
                "TheHunter",    "jlouis"
        ]
   }
