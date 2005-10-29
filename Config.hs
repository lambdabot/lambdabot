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
        textwidth :: Int,           -- ^ How many columns should we use
        autojoin  :: [String],      -- ^ List of channels to autojoin
        admins    :: [String],      -- ^ List of nicknames that are admins
        proxy     :: Maybe ([Char], Integer), -- ^ A proxy given as
	                                      --   a pair of host and port.

        hooglePath :: FilePath      -- ^ path to hoogle directory
}

--
-- Useful defaults for #haskell.
--
config :: Config
config = Config {
        name            = "lambdabot",
        userinfo        = "Lambda Robots - 100% Loyal",
        host            = "irc.au.freenode.net",

        port            = 6667,
        verbose         = True,
        textwidth       = 65,
        moresize        = 6,
        proxy           = Just ("www-proxy",3128),
        autojoin        = ["#haskell","#haskell-blah"
                 	  ,"#haskell-overflow","#gentoo-haskell"],

        admins          = [
                "Pseudonym",    "shapr", "Heffalump",    "Igloo",  
                "Cale",         "dons", "TheHunter",    "jlouis"
        ],

        hooglePath      = "/home/dons/hoogle"
   }

