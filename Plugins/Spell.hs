--
-- Copyright (c) 2004-5 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- | Interface to /aspell/, an open source spelling checker, from a
-- suggestion by Kai Engelhardt. Requires you to install aspell.
--
module Plugins.Spell (theModule) where

import Spell                    (spell)
import Util                     (showClean)
import Lambdabot
import Control.Monad.Trans      ( liftIO )

newtype SpellModule = SpellModule ()

theModule :: MODULE
theModule = MODULE $ SpellModule ()

instance Module SpellModule () where
    moduleHelp _ _    = "@spell <word>, show spelling of word"
    moduleCmds   _    = ["spell"]

    process _ _ src "spell" [] = ircPrivmsg src "No word to spell."
    process _ _ src "spell" s = do
        ss <- liftIO (spell s)
        ircPrivmsg src (showClean $ take 5 ss)
    process _ _ _ _ _         = error "SpellModule: invalid command"
