module QuoteModule where

import Control.Monad.State
import Data.Dynamic
import Data.IORef
import Data.FiniteMap
import QuoteModule.Fortune
import QuoteModule.Yow
import IRC
import Maybe
import System.Random
import System.Time
-- 	$Id: QuoteModule.hs,v 1.1 2003/07/29 13:41:48 eleganesh Exp $	

newtype QuoteModule = QuoteModule ()

theModule = MODULE quoteModule
quoteModule = QuoteModule ()

instance Module QuoteModule where
    moduleName   m = return "quote"
    moduleSticky m = False
    commands     m = return ["fortune","yow","arr"]
    process      m msg target cmd rest
      = do 
        maybemyref <- gets (\s -> lookupFM (ircModuleState s) "prngint")
        case maybemyref of
                        Just myref -> do modstate <- liftIO (readIORef myref)
                                         let quotefun = case cmd of
                                                                 "fortune" -> randFortune
                                                                 "yow"     -> yowRandom
                                                                 "arr"     -> arrRandom
                                         quoteGenPair <- liftIO (quotefun $ mkStdGen (stripMS modstate))
                                         liftIO (writeIORef myref (ModuleState ((genToInt . snd) quoteGenPair)))
                                         ircPrivmsg target (fst quoteGenPair)
                        -- init the state for this module if it doesn't exist
                        Nothing    -> do s <- get
                                         i <- liftIO (liftM castMe intGet)
                                         newref <- liftIO (newIORef (ModuleState (i :: Int)))
                                         let statemap = ircModuleState s
                                         put (s { ircModuleState 
                                                  = addToFM statemap "prngint" newref })
                                         process m msg target cmd rest


genToInt x = fst (next x)

-- random seed from picoseconds, suggested by Marvin--
maxI = (maxBound :: Int)

intGet = do calTime <- liftM toCalendarTime getClockTime
            bigNum <- liftM ctPicosec calTime
            return (until (< (fromIntegral maxI)) sub2int bigNum)

castMe :: Integer -> Int
castMe x = fromIntegral x
sub2int :: Integer -> Integer
sub2int x = (x - (fromIntegral maxI) - (fromIntegral maxI))

-- TODO: refactor the list chooser code out of Yow/Fortune into a common piece of code
--       figure out what the magic numbers do, and document them

arrRandom rng
    = do
      return (QuoteModule.Yow.getRandItem arrList rng)
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