-- | Configuration data for lambdabot
module Lambdabot.Config where

-- | The 'Config' type provides configurations for lambdabot. It is used
--   when lambdabot is started to determine the name of lambdabot, what
--   IRC-network lambdabot should join, which channels lambdabot should
--   join upon successful connection, etc.
--
data Config = Config
    { verbose           :: Bool
    , proxy             :: Maybe ([Char], Integer) 
        -- ^ An HTTP proxy given as a pair of host and port.
    , ghci              :: FilePath
        -- ^ which ghci to use (in "\@type")
    , outputDir         :: FilePath
    , commandPrefixes   :: [String]
        -- ^ what prefixes to use for commands
    , evalPrefixes      :: [String]
        -- ^ what prefixes to use for Haskell evalution
    , disabledCommands  :: [String]
        -- ^ Particular commands we'd like to disable
        -- (to disable whole plugins, remove them from Modules.hs)
    }

-- | Useful defaults for #haskell.
defaultConfig :: Config
defaultConfig = Config
    { verbose                 = False
    , proxy                   = Nothing
    , ghci                    = "ghci"
    , outputDir               = "State/"
    , commandPrefixes         = ["@","?"]
    , evalPrefixes            = [">"]
    , disabledCommands        = []
    }
