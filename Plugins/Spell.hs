--
-- Copyright (c) 2004-5 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- | Interface to /aspell/, an open source spelling checker, from a
-- suggestion by Kai Engelhardt. Requires you to install aspell.
--
module Plugins.Spell (theModule) where

import Spell                    (spell)
import Util                     (showClean)
import Lambdabot

newtype SpellModule = SpellModule ()

theModule :: MODULE
theModule = MODULE $ SpellModule ()

instance Module SpellModule () where
    moduleCmds   _  = ["spell"]
    moduleHelp _ _  = "@spell <word>, show spelling of word"
    process_ _ _ [] = return ["No word to spell."]
    process_ _ _ s  = do ss <- liftIO (spell s) ; return [showClean $ take 5 ss]
