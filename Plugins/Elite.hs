-- (c) Josef Svenningsson, 2005
-- Licence: No licence, public domain
--
-- Inspired by the following page:
-- http://www.microsoft.com/athome/security/children/kidtalk.mspx
module Plugins.Elite (theModule) where

import Lambdabot
import Util

import Control.Monad.State
import Data.Char
import Text.Regex

theModule :: MODULE
theModule = MODULE $ Elite

data Elite = Elite

instance Module Elite () where
    moduleCmds _ = ["elite"]
    moduleHelp _ cmd = case cmd of
                        "elite" -> "Translating english to elitespeak"
                        _       -> "Translations between english and elitespeak"
    process _ _ target _ args
        = do result <- case words args of
                         [] -> return "Say again?"
                         wds -> do let instr = map toLower (unwords wds)
                                   transWords <- translate instr
                                   return transWords
             ircPrivmsg target result

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
                map (\ (regex, subst) -> (matchRegexAll regex str,subst)) $
                ruleList)
               ++ [([toUpper $ head str],tail str)
                  ,([head str],tail str)
                  ]
               

isJustEmpty :: (Maybe([a],b,c,d),e) -> Bool
isJustEmpty (Just ([],_,_,_),_) = True
isJustEmpty (_,_)                = False

ruleList :: [(Regex,String)]
ruleList = map (\ (regex,subst) -> (mkRegex regex,subst))
           [("a","4")
           ,("b","8")
           ,("c","(")
           ,("ck","xx")
           ,("cks ","x ")
           ,(" cool "," kewl ")
           ,("e","3")
           ,("elite","1337")
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
           ,("v","\\/")
           ,("w","\\/\\/")
           ,(" you "," u ")
           ,(" you "," joo ")
           ,("z","s")
           ]

