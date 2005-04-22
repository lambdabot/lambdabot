--
-- | Configuration data for lambdabot
--
module Config where

-- | The 'Config' type provides configurations for lambdabot. It is used
--   when lambdabot is started to determine the name of lambdabot, what
--   IRC-network lambdabot should join, which channels lambdabot should
--   join upon successful connection, etc.
--
data Config = Config {
        name      :: String,        -- ^ The nickname of lambdabot
        userinfo  :: String,        -- ^ The userinfo string for lambdabot
        host      :: String,        -- ^ Host to join
        port      :: Int,           -- ^ The port number to use on the host
        verbose   :: Bool,          -- ^ Should lambdabot be verbose?
        moresize  :: Int,           -- ^ How many lines is output before \@more?
        autojoin  :: [String],      -- ^ List of channels to autojoin
        admins    :: [String],      -- ^ List of nicknames that are admins
        proxy     :: Maybe ([Char], Integer) -- ^ An eventual proxy given as
	                                         --   a pair of host and port.
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

--
-- | This type describes object dependencies required at runtime.
-- It is calculated at build time with 'make Depends.conf', and used
-- by the Dynamic module, and GenModules
--
data Depends = Depends {
        reqObjs :: [String],           -- ^ a common subset of modules 
        reqPkgs :: [String],           -- ^ all required packages
        depList :: [(String,[String])] -- ^ assoc list of modules and their obj deps
    }
    deriving (Show,Read)

-- | An association between modules and their dependencies
type DepList = [(String,[String])]
