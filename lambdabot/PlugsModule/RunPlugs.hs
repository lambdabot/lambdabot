--
-- Copyright (c) 2004 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- requires the 'runplugs' utility available with the hs-plugins library.
-- in $hsplugins/examples/hmake/one-shot
--

module PlugsModule.RunPlugs where

import Posix            ( popen )

binary :: String
binary = "runplugs"

--
-- return stdout. ignore stderr. this might not be desirable
--
plugs :: String -> IO String
plugs src =
        do (o,_,_) <- popen "runplugs" [] (Just src)
           let o' = clean o
           return $ if null o' then "bzzt\n" else o'

--
-- didn't compile. could return a useful message.
--
clean :: String -> String
clean s = case s of 
        ('\n':'<':'P':'l':'u':'g':'i':'n':'s':_) -> []
        _ -> s

