module Lib.Regex (
        -- ByteString interface
        regex,      -- :: ByteString -> Regex 
        matches,    -- :: Regex -> ByteString -> Bool

        -- String interface
        regex',     -- :: String -> Regex 
        matches',   -- :: Regex -> String -> Bool

        -- and the underlying module
        module Text.Regex.Posix.ByteString

    ) where

import Text.Regex.Posix.ByteString
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Char8

--
-- convenient regex wrappers:
--
regex' :: String -> Regex
regex' s = regex (pack s)

regex :: ByteString -> Regex
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
matches' :: Regex -> String -> Bool
matches' r s = matches r (pack s)

matches  :: Regex -> ByteString -> Bool
matches r p = unsafePerformIO $ do
    res <- execute r p
    case res of
        Left err       -> error $ "regex execute failed: " ++ show err
        Right Nothing  -> return False
        Right (Just _) -> return True

