-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

--
-- Implements a preprocessor for plugins, filling in commonly required
-- syntax.
--
-- Currently only useful for plugins that only:
--  import Plugin
-- and have () for state.  
--

import System.Environment

main = do
    [_,i,o] <- getArgs
    readFile i >>= writeFile o . expand

expand [] = []
expand ('P':'L':'U':'G':'I':'N':' ':xs) = render name ++ tail rest
    where (name, rest) = break (=='\n') xs
expand (x:xs) = x : expand xs

render name =
   "newtype "++name++"Module = "++name++"Module () \n\ 
    \\n\ 
    \theModule :: MODULE                            \n\ 
    \theModule = MODULE $ "++name++"Module ()       \n"
