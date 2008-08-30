{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

-- | Haskell project name generation
-- semi-joke
module Plugin.Fresh (theModule) where

import Plugin

$(plugin "Fresh")

instance Module FreshModule Integer where
    moduleCmds      _ = ["freshname"]
    moduleHelp    _ _ = "freshname. Return a unique Haskell project name."
    moduleDefState  _ = return 0
    moduleSerialize _ = Just stdSerial
    process_ _ _ _    = withMS $ \n f -> do
                            f (n+1)
                            return ["Ha" ++ reverse (freshname n)]

freshname :: Integer -> String
freshname i
    | i == 0    = [chr (ord 'a')]
    | r == 0    = [chr (ord 'a' + (fromIntegral a))]
    | otherwise =  chr (ord 'a' + (fromIntegral a)) : freshname r
    where
      (r,a) = i `quotRem` 26
