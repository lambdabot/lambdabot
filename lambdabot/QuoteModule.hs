{-# OPTIONS -fglasgow-exts #-}
-- 	$Id: QuoteModule.hs,v 1.1 2003/07/29 13:41:48 eleganesh Exp $

module QuoteModule where

import QuoteModule.Fortune      (randFortune)
import Util                     (stdGetRandItem)
import IRC

import Control.Monad.State

------------------------------------------------------------------------
newtype QuoteModule = QuoteModule ()

theModule :: MODULE
theModule = MODULE quoteModule

quoteModule :: QuoteModule
quoteModule = QuoteModule ()

instance Module QuoteModule () where
    moduleName   _ = return "quote"
    moduleHelp _ "fortune" = return "Provide a random fortune"
    moduleHelp _ "yow"     = return "Yow!"
    moduleHelp _ "arr"     = return "Talk to a pirate"
    moduleHelp _ _         = return "The quote module provides a range of quotes"
    moduleSticky _ = False
    commands     _ = return ["fortune","yow","arr"]
    process      _ _ target cmd _
      = do quote <- liftIO $ case cmd of
                      "fortune" -> randFortune Nothing
                      "yow"     -> randFortune (Just "zippy")
                      "arr"     -> arrRandom
                      _ -> error "QuoteModule: bad string"
           ircPrivmsg target quote

-- | Return a random arr-quote
arrRandom :: IO String
arrRandom = Util.stdGetRandItem arrList

-- | A list of arr-quotes
arrList :: [String]
arrList = [
           "Avast!"
          ,"Shiver me timbers!"
          ,"Yeh scurvy dog..."
          ,"I heard andersca is a pirate"
          ,"I'll keel haul ya fer that!"
          ,"I'd like to drop me anchor in her lagoon"
          ,"Well me hearties, let's see what crawled out of the bung hole..."
          ,"I want me grog!"
          ,"Drink up, me hearties"
          ,"Is that a hornpipe in yer pocket, or arr ya just happy ta see me?"
          ,"Get out of me way, yeh landlubber"
          ,"Smartly me lass"
          ,"Arrr!"
          ,"Ahoy mateys"
          ,"Aye"
          ,"Aye Aye Cap'n"
          ,"This is the END for you, you gutter-crawling cur!"
          ,"May the clap make ye incapable of Cracking Jenny's Tea Cup."
          ,"Eat maggoty hardtack, ye unkempt, jenny frequentin', son of a gun."
          ]
