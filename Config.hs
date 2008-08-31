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
        verbose   :: Bool,          -- ^ Should lambdabot be verbose?
        textwidth :: Int,           -- ^ How many columns should we use
        proxy     :: Maybe ([Char], Integer), -- ^ A proxy given as
                                              --   a pair of host and port.

        -- | The 'path' component is a string to the location where the fortune files
        --   are located. On some systems, this is "\/usr\/share\/games\/fortunes", on others
        --   this is "\/usr\/share\/games\/fortune". Alter this to suit your configuration
        fortunePath :: FilePath,

        -- | Path to the top of "\$fptools", used by "\@code"
        fptoolsPath :: FilePath,

        -- | which ghci to use (in "\@type")
        ghci        :: FilePath,
        outputDir   :: FilePath,

        -- | what prefixes to use for commands
        commandPrefixes :: [String],

        -- | what prefixes to use for Haskell evalution
        evalPrefixes :: [String],

        -- | Particular commands we'd like to disable
        -- (to disable whole plugins, remove them from Modules.hs)
        disabledCommands :: [String]
}

--
-- Useful defaults for #haskell.
--
config :: Config
config = Config {
        verbose         = True,
        textwidth       = 350,
        proxy           = Nothing, -- Just ("www-proxy",3128),

        fortunePath     = "/usr/share/games/fortunes/",
        fptoolsPath     = "/home/dons/fptools",

        ghci            = "ghci",
        outputDir       = "State/",

        commandPrefixes = ["@","?"],
        evalPrefixes    = [">"],

        disabledCommands = ["state"]

   }
