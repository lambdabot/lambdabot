{-# LANGUAGE TemplateHaskell #-}
-- (c) Josef Svenningsson, 2005
-- Licence: No licence, public domain

-- Inspired by the following page:
-- http://www.microsoft.com/athome/security/children/kidtalk.mspx
module Plugin.Elite (theModule) where

import Plugin
import qualified Text.Regex as R

import Control.Arrow

plugin "Elite"

instance Module EliteModule where
    moduleCmds _ = 
        [ (command "elite")
            { aliases = ["leet", "l33t", "1337"]
            , help = say "elite <phrase>. Translate English to elitespeak"
            , process = \args -> case words args of
                 [] -> say "Say again?"
                 wds -> do let instr = map toLower (unwords wds)
                           say =<< translate instr
            }
        ]

translate :: MonadIO m => String -> m String
translate []  = return []
translate str = case alts of
                  [] -> do rest <- translate (tail str)
                           return (head str : rest)
                  _ -> do (subst,rest) <- liftIO $ stdGetRandItem alts
                          translatedRest <- translate rest
                          return (subst ++ translatedRest)
  where alts = (map (\ (Just (_,_,rest,_),subst) -> (subst,rest)) $
                filter isJustEmpty $
                map (\ (re, subst) -> (R.matchRegexAll re str,subst)) $
                ruleList)
               ++ [([toUpper $ head str],tail str)
                  ,([head str],tail str)
                  ]

isJustEmpty :: (Maybe([a],b,c,d),e) -> Bool
isJustEmpty (Just ([],_,_,_),_) = True
isJustEmpty (_,_)                = False

ruleList :: [(Regex,String)]
ruleList = map (first regex')
           [("a","4")
           ,("b","8")
           ,(" be "," b ")
           ,("c","(")
           ,("ck","xx")
           ,("cks ","x ")
           ,("cks ","x0rs ")
           ,("cks ","x0rz ")
           ,(" cool "," kewl ")
           ,("e","3")
           ,("elite","1337")
           ,("elite","leet")
           ,("f","ph")
           ,(" for "," 4 ")
           ,("g","9")
           ,("h","|-|")
           ,("k","x")
           ,("l","|")
           ,("l","1")
           ,("m","/\\/\\")
           ,("o","0")
           ,("ph","f")
           ,("s","z")
           ,("s","$")
           ,("s","5")
           ,("s ","z0rz ")
           ,("t","7")
           ,("t","+")
           ,(" to "," 2 ")
           ,(" to "," too ")
           ,(" too "," to ")
           ,("v","\\/")
           ,("w","\\/\\/")
           ,(" you "," u ")
           ,(" you "," yu ")
           ,(" you "," joo ")
           ,("z","s")
           ]

