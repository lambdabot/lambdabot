-----------------------------------------------------------------------------
-- |
-- Module      :  Lambdabot.Regex
-- Copyright   :  (c) Don Stewart 2007
-- License     :  GPL (see LICENSE)
--
-- Maintainer  :  dons@galois.com
--
-----------------------------------------------------------------------------

module Lambdabot.Regex (
        -- ByteString interface
        regex,      -- :: ByteString -> Regex
        matches,    -- :: Regex -> ByteString -> Bool

        -- String interface
        regex',     -- :: String -> Regex
        matches',   -- :: Regex -> String -> Bool

        -- and the underlying module
        module Text.Regex.Posix.ByteString

    ) where

import Data.ByteString.Char8

import Text.Regex.Posix.ByteString
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
--
-- convenient regex wrappers:
--

regex'   ::     String -> Regex
regex    :: ByteString -> Regex

matches' :: Regex ->     String -> Bool
matches  :: Regex -> ByteString -> Bool

------------------------------------------------------------------------

-- For ghc 6.6 we use the regex-posix bytestring library

regex' s = regex (pack s)

regex p = unsafePerformIO $ do
    res <- compile compileFlags execFlags p
    case res of
        Left  err -> error $ "regex failed: " ++ show err
        Right r   -> return r
  where
    compileFlags = compExtended
    execFlags    = 0

-- match a regex against a string or bytestring
matches' r s = matches r (pack s)

matches r p = unsafePerformIO $ do
    res <- execute r p
    case res of
        Left err       -> error $ "regex execute failed: " ++ show err
        Right Nothing  -> return False
        Right (Just _) -> return True
