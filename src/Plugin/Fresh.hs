{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

-- | Haskell project name generation
-- semi-joke
module Plugin.Fresh (theModule) where

import Plugin

$(plugin "Fresh")

instance Module FreshModule where
    type ModuleState FreshModule = Integer
    
    moduleCmds _ =
        [ (command "freshname")
            { help = say "freshname. Return a unique Haskell project name."
            , process = \_ -> lift fresh >>= say
            }
        ]

fresh :: Fresh String
fresh = withMS $ \n f -> do
    f (n+1)
    return ("Ha" ++ reverse (asName n))

asName :: Integer -> String
asName i
    | i == 0    = [chr (ord 'a')]
    | r == 0    = [chr (ord 'a' + (fromIntegral a))]
    | otherwise =  chr (ord 'a' + (fromIntegral a)) : asName r
    where
      (r,a) = i `quotRem` 26
