--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- requires the 'runplugs' utility available with the hs-plugins library.
-- in $hsplugins/examples/hmake/one-shot
--

--
-- a Haskell evaluator for the pure part, using `plugs`
--

module PlugsModule where

import IRC      hiding ( clean ) 
import PosixCompat
import Control.Monad.Trans      ( liftIO )

newtype PlugsModule = PlugsModule ()

theModule :: MODULE
theModule = MODULE plugsModule

plugsModule :: PlugsModule
plugsModule = PlugsModule ()

instance Module PlugsModule where
        moduleName   _ = return "plugs"
        moduleHelp _ _ = return "@plugs <expr>\nYou have Haskell, 3 seconds and no IO. Go nuts!"
        moduleSticky _ = False
        commands     _ = return ["plugs"]
        process _ _ src "plugs" s = do o <- liftIO $ plugs s 
                                       ircPrivmsg src o
        process _ _ _ _ _ = error "PlugsModule: invalid command"

binary :: String
binary = "runplugs"

--
-- return stdout. ignore stderr. this might not be desirable
--
plugs :: String -> IO String
plugs src =
        do (o,p,_) <- popen binary [] (Just src)
           let o' = clean o
           return $ if null o' 
                        then let p' = clean p 
                             in if null p' then "bzzt\n" else p'
                        else o'

--
-- didn't compile. could return a useful message.
--
clean :: String -> String
clean s = case s of 
        ('\n':'<':'P':'l':'u':'g':'i':'n':'s':_) -> []
        _ -> s

