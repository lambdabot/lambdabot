{-# OPTIONS -O -funbox-strict-fields #-}
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
-- Also used to generate the modules list in Modules.hs
--

import System.Environment

import qualified Data.ByteString.Char8 as B -- crank it up, yeah!

import BotLib

main :: IO ()
main = do
    -- putStr "BotPP called with args: ";  print =<< getArgs
    [orig,i,o] <- getArgs
    let basename = baseName orig
    -- putStr "basename = "; print basename
    B.readFile i >>= \l -> B.writeFile o $ expand (B.length l) 0 basename l
