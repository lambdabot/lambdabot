module Plugins.Hoogle (theModule) where

import Lambdabot
import PosixCompat
import Control.Monad.Trans ( liftIO )

newtype HoogleModule = HoogleModule ()

theModule :: MODULE
theModule = MODULE $ HoogleModule ()

instance Module HoogleModule () where
    moduleSticky _ = False

    moduleHelp _ _ = return "@hoogle <expr>, Haskell API Search for either names, or types."

    moduleCmds   _ = return ["hoogle"]

    process _ _ src "hoogle" s = do o <- liftIO $ hoogle s
                                    ircPrivmsg src o
    process _ _ _ _ _ = error "HoogleModule: invalid command"

binary :: String
binary = "/home/paolo/code/haskell/hoogle/hoogle"

hoogle :: String -> IO String
hoogle src = do (out,err,_) <- popen binary [src] (Just "")
                return $ result out err
    where result [] [] = "Terminated\n"
          result [] ys = ys
          result xs _  = takeWhile (/='\n') xs
