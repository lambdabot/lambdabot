--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- Talk to Neil Mitchell's `Hoogle' program
--

module Plugins.Hoogle (theModule) where

import Lambdabot
import PosixCompat
import LBState
import Serial 
import Util
import Data.List
import qualified Config

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

hoogleBinary :: FilePath
hoogleBinary = Config.hooglePath Config.config </> "hoogle"

hoogleText :: FilePath
hoogleText = Config.hooglePath Config.config </> "src" </> "hoogle.txt"

-- arbitrary cutoff point
cutoff :: Int
cutoff = -10

-- | Actually run the hoogle binary
hoogle :: String -> IO [String]
hoogle query = do 
        let args = ["--count", "20"
                   ,"-l", hoogleText
                   ,"--verbose"
                   ,query]         
        (out,err,_) <- popen hoogleBinary args (Just "")
        return $ result out err

    where result [] [] = ["An error occured.\n"]
          result [] ys = [ys]
          result xs _  = 
		let xs' = map toPair $ lines xs
		    res = map snd $ filter ((>=cutoff) . fst) xs'
		in if null res
                   then ["No matches, try a more general search"]
		   else sortBy qualifiedName res

          qualifiedName x y = 
                   let (n,_) = break (== ':') x
                       (m,_) = break (== ':') y
                   in m `compare` n

	  toPair s = let (res, meta)  = break (=='@') s
		         rank = takeWhile (/=' ') . drop 2 $ meta
	             in case readM rank :: Maybe Int of
				Just n  -> (n,res)
		                Nothing -> (0,res)

