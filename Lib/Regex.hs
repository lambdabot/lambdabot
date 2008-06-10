{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Lib.Regex
-- Copyright   :  (c) Don Stewart 2007
-- License     :  GPL (see LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
--
-----------------------------------------------------------------------------

module Lib.Regex (
        -- ByteString interface
        regex,      -- :: ByteString -> Regex
        matches,    -- :: Regex -> ByteString -> Bool

        -- String interface
        regex',     -- :: String -> Regex
        matches',   -- :: Regex -> String -> Bool

        -- and the underlying module
#if __GLASGOW_HASKELL__ >= 606
        module Text.Regex.Posix.ByteString
#else
        module Text.Regex
#endif

    ) where

import Data.ByteString.Char8

#if __GLASGOW_HASKELL__ >= 606
import Text.Regex.Posix.ByteString
import System.IO.Unsafe (unsafePerformIO)
#else
import Text.Regex
#endif

------------------------------------------------------------------------
--
-- convenient regex wrappers:
--

regex'   ::     String -> Regex
regex    :: ByteString -> Regex

matches' :: Regex ->     String -> Bool
matches  :: Regex -> ByteString -> Bool

------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 606

--
-- For ghc 6.6 we use the regex-posix bytestring library
--

regex' s = regex (pack s)

regex p = unsafePerformIO $ do
    res <- compile compileFlags execFlags p
    case res of
        Left  err -> error $ "regex failed: " ++ show err
        Right r   -> return r
  where
    compileFlags = compExtended
    execFlags    = 0

--
-- match a regex against a string or bytestring
--
matches' r s = matches r (pack s)

matches r p = unsafePerformIO $ do
    res <- execute r p
    case res of
        Left err       -> error $ "regex execute failed: " ++ show err
        Right Nothing  -> return False
        Right (Just _) -> return True

#else

--
-- ghc 6.4.x Text.Regex compat:
--

regex'  = mkRegex
regex   = regex' . unpack

matches' r s | Just _ <- matchRegex r s = True
             | otherwise                = False

matches  r s = matches' r (unpack s)

#endif
