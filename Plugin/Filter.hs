{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
-- | GNU Talk Filters
-- needs: http://www.hyperrealm.com/main.php?s=talkfilters
-- Edward Kmett 2006

module Plugin.Filter where

import Plugin

$(plugin "Filter")

instance Module FilterModule () where
        moduleCmds _   = ["austro","b1ff","brooklyn","chef","cockney","drawl","dubya","fudd","funetak","jethro","jive","kraut","pansy","pirate","postmodern","redneck","valspeak","warez"]
        moduleHelp _ "austro" = "austro <phrase>. Talk like Ahhhnold"
        moduleHelp _ "b1ff" = "b1ff <phrase>. B1ff of usenet yore"
        moduleHelp _ "brooklyn" = "brooklyn <phrase>. Yo"
        moduleHelp _ "chef" = "chef <phrase>. Bork bork bork"
        moduleHelp _ "cockney" = "cockney <phrase>. Londoner accent"
        moduleHelp _ "drawl" = "drawl <phrase>. Southern drawl"
        moduleHelp _ "dubya" = "dubya <phrase>. Presidential filter"
        moduleHelp _ "fudd" = "fudd <phrase>. Fudd, Elmer"
        moduleHelp _ "funetak" = "funetak <phrase>. Southern drawl"
        moduleHelp _ "jethro" = "jethro <phrase>. Now listen to a story 'bout a man named Jed..."
        moduleHelp _ "jive" = "jive <phrase>. Slap ma fro"
        moduleHelp _ "kraut" = "kraut <phrase>. German accent"
        moduleHelp _ "pansy" = "pansy <phrase>. Effeminate male"
        moduleHelp _ "pirate" = "pirate <phrase>. Talk like a pirate"
        moduleHelp _ "postmodern" = "postmodern <phrase>. Feminazi"
        moduleHelp _ "redneck" = "redneck <phrase>. Deep south"
        moduleHelp _ "valspeak" = "valley <phrase>. Like, ya know?"
        moduleHelp _ "warez" = "warez <phrase>. H4x0r"
        process_ _ f s = do
                let usage = ["usage: " ++ f ++ " <phrase>"]
                case words s of
                        [] -> return usage
                        t -> io (runFilter f (unwords t))

pathTo :: String -> String
pathTo f = "/usr/local/bin/" ++ f

runFilter :: String -> String -> IO [String]
runFilter f s = do
        (out,_,_) <- popen (pathTo f) [] (Just s)
        return $ result out
    where result [] = ["Couldn't run the filter."]
          result xs = filter (not . all (==' ')) . lines $ xs
