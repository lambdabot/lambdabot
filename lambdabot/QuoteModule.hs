module QuoteModule where

import qualified Map as M
import Control.Monad.State
import Data.IORef
import QuoteModule.Fortune
import QuoteModule.Random
import IRC
import Maybe
import System.Random
import System.Time
-- 	$Id: QuoteModule.hs,v 1.1 2003/07/29 13:41:48 eleganesh Exp $

newtype QuoteModule = QuoteModule ()

theModule :: MODULE
theModule = MODULE quoteModule

quoteModule :: QuoteModule
quoteModule = QuoteModule ()

instance Module QuoteModule where
    moduleName   _ = return "quote"
    moduleHelp _ "fortune" = return "Provide a random fortune"
    moduleHelp _ "yow"     = return "Yow!"
    moduleHelp _ "arr"     = return "Talk to a pirate"
    moduleHelp _ _         = return "The quote module provides a range of quotes"
    moduleSticky _ = False
    commands     _ = return ["fortune","yow","arr"]
    process      m msg target cmd rest
      = do
        maybemyref <- gets (\s -> M.lookup "prngint" (ircModuleState s))
        case maybemyref of
          Just myref -> do modstate <- liftIO (readIORef myref)
                           let quotefun =
                                 case cmd of
                                 "fortune" -> randFortune Nothing
                                 "yow"     -> randFortune (Just "zippy")
                                 "arr"     -> arrRandom
                                 _ -> error "QuoteModule: bad string"
                           (quote, newseed) <- liftIO
                                                (quotefun $
						  mkStdGen (stripMS modstate))
                           liftIO (writeIORef myref
				              (ModuleState $ genToInt newseed))
                           ircPrivmsg target quote
                        -- init the state for this module if it doesn't exist
          Nothing    -> do s <- get
                           i <- liftIO (liftM castMe intGet)
                           newref <- liftIO
			               (newIORef (ModuleState (i :: Int)))
                           let statemap = ircModuleState s
                           put (s { ircModuleState = M.insert "prngint"
				                              newref
					                      statemap })
                           process m msg target cmd rest


genToInt :: (RandomGen g) => g -> Int
genToInt x = fst (next x)

-- random seed from picoseconds, suggested by Marvin--
maxI :: Int
maxI = (maxBound :: Int)

intGet :: IO Integer
intGet = do calTime <- liftM toCalendarTime getClockTime
            bigNum <- liftM ctPicosec calTime
            return (until (< (fromIntegral maxI)) sub2int bigNum)

castMe :: Integer -> Int
castMe x = fromIntegral x

sub2int :: Integer -> Integer
sub2int x = (x - (fromIntegral maxI) - (fromIntegral maxI))

arrRandom :: (Monad m, RandomGen g) => g -> m ([Char], g)
arrRandom rng = return (QuoteModule.Random.getRandItem arrList rng)

arrList :: [[Char]]
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
