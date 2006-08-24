--
-- | Configuration data for lambdabot
--
module Config where

data Protocol = Irc | Xmpp

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
        protocol  :: Protocol,      -- ^ either irc or xmpp/jabber
        verbose   :: Bool,          -- ^ Should lambdabot be verbose?
        textwidth :: Int,           -- ^ How many columns should we use
        autojoin  :: [String],      -- ^ List of channels to autojoin
        admins    :: [String],      -- ^ List of nicknames that are admins
        proxy     :: Maybe ([Char], Integer), -- ^ A proxy given as
                                              --   a pair of host and port.

        -- | The 'path' component is a string to the location where the fortune files
        --   are located. On some systems, this is /usr/share/games/fortunes, on others
        --   this is /usr/share/games/fortune. Alter this to suit your configuration
        fortunePath :: FilePath,

        -- | Path to the top of $fptools, used by @code
        fptoolsPath :: FilePath,

        -- which ghci to use (in @type)
        ghci        :: FilePath,
        outputDir   :: FilePath,

        -- what prefixes to use for commands
        commandPrefixes :: [String],

        -- what prefixes to use for Haskell evalution
        evalPrefixes :: [String]
}

--
-- Useful defaults for #haskell.
--
config :: Config
config = Config {
        name            = "lambdabot",
        userinfo        = "Lambda_Robots:_100%_Loyal",
        host            = "chat.freenode.net",
        protocol        = Irc,

        port            = 6667,
        verbose         = True,
        textwidth       = 350,
        proxy           = Just ("www-proxy",3128),
        autojoin        = ["#haskell","#haskell-blah","#flippi"
                          ,"#haskell-overflow","#gentoo-haskell"
                          ,"#haskell_ru", "#darcs" ,  "#perl6"
                          ,"#haskell.it","#haskell.se", "#haskell.es","#ScannedInAvian"],


        admins          = [
                "Pseudonym",    "shapr", "vincenz",    "Igloo",
                "Cale",         "dons", "TheHunter",    "musasabi", "Lemmih"
        ],

        fortunePath     = "/home/dons/fortune/",
        fptoolsPath     = "/home/dons/fptools",

        ghci            = "ghci",
        outputDir       = "State/",

        commandPrefixes = ["@","?"],
        evalPrefixes   = [">"]
   }
