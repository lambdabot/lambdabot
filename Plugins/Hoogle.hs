module Plugins.Hoogle (theModule) where

import Lambdabot
import PosixCompat
import LBState

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
binary = "/home/thomas/hoogle/hoogle"

hoogle :: String -> IO [String]
hoogle src = do (out,err,_) <- popen binary [src] (Just "")
                return $ result out err
    where result [] [] = ["Terminated\n"]
          result [] ys = [ys]
          result xs _  = lines xs

