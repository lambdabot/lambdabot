-- | Fortune.hs, quote the fortune file
module Plugin.Quote.Fortune (randFortune) where

import Plugin
import Control.Exception (bracket)
import Data.Fortune
import qualified Data.Text as T

defaultSections :: IO [FilePath]
defaultSections = defaultFortuneFiles False False

fortuneRandom :: FilePath -> IO String
fortuneRandom path = bracket
    (openFortuneFile path '%' False)
    closeFortuneFile
    (fmap T.unpack . randomFortune)

resolve :: FilePath -> IO FilePath
resolve path = do
    fortDir <- getFortuneDir
    return (fortDir </> path)

-- | Given an optional fortune section, return a random fortune. If Nothing,
--   then a random fortune from all fortune files is returned. If Just section,
--   then a random fortune from the given section is returned.
randFortune :: [FilePath] -> IO String
randFortune []      = defaultSections      >>= randomElem >>= fortuneRandom
randFortune section = mapM resolve section >>= randomElem >>= fortuneRandom
