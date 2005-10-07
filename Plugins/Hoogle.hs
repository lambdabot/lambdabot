--
-- Talk to Neil Mitchell's `Hoogle' program
--

module Plugins.Hoogle (theModule) where

import Lambdabot
import PosixCompat
import LBState
import Util

import Control.Monad	   ( when )
import Control.Monad.Trans ( liftIO )

newtype HoogleModule = HoogleModule ()

theModule :: MODULE
theModule = MODULE $ HoogleModule ()

type HoogleState = [String]

instance Module HoogleModule HoogleState where
    moduleSticky   _ = False
    moduleDefState _ = return []

    moduleHelp _ _ = return "@hoogle <expr>, Haskell API Search for either names, or types."
    moduleCmds   _ = return ["hoogle", "hoogle+"]

    process _ _ src "hoogle" s = do 
	o <- liftIO $ hoogle s
	let (this,that) = splitAt 3 o
	writeMS that
	ircPrivmsg src (unlines this)

    process  _ _ src "hoogle+" _ = do
	this <- withMS $ \st write -> do
			let (this,that) = splitAt 3 st
			write that
			return this
	when (not . null $ this) $
		ircPrivmsg src (unlines this)

    process _ _ _ _ _ = error "HoogleModule: invalid command"

binary :: String
binary = "/home/dons/bin/hoogle"

cutoff :: Int
cutoff = -10

hoogle :: String -> IO [String]
hoogle src = do (out,err,_) <- popen binary ["-v",src] (Just "")
                return $ result out err
    where result [] [] = ["Terminated\n"]
          result [] ys = [ys]
          result xs _  = 
		let xs' = map toPair $ lines xs
		    res = map snd $ filter ((>=cutoff) . fst) xs'
		in if null res
                   then ["No matches, try a more general search"]
		   else res

	  toPair s = let (rank,res) = break (==':') s
		         res' = dropWhile (==':') res
	             in case readM rank :: Maybe Int of
				Just n -> (n,res')
		                Nothing -> (0,res')

