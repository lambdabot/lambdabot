{-# LANGUAGE TemplateHaskell #-}
-- | GNU Talk Filters
-- needs: http://www.hyperrealm.com/main.php?s=talkfilters
-- Edward Kmett 2006

module Plugin.Filter (theModule) where

import Plugin

plugin "Filter"

instance Module FilterModule where
        moduleCmds _ = 
            [ (command name)
                { help = say descr
                , process = \s -> do
                    f <- getCmdName
                    let usage = "usage: " ++ f ++ " <phrase>"
                    case words s of
                            [] -> say usage
                            t -> io (runFilter f (unwords t)) >>= mapM_ say
                }
            | (name, descr) <- filters
            ]

filters = 
    [ ("austro",     "austro <phrase>. Talk like Ahhhnold")
    , ("b1ff",       "b1ff <phrase>. B1ff of usenet yore")
    , ("brooklyn",   "brooklyn <phrase>. Yo")
    , ("chef",       "chef <phrase>. Bork bork bork")
    , ("cockney",    "cockney <phrase>. Londoner accent")
    , ("drawl",      "drawl <phrase>. Southern drawl")
    , ("dubya",      "dubya <phrase>. Presidential filter")
    , ("fudd",       "fudd <phrase>. Fudd, Elmer")
    , ("funetak",    "funetak <phrase>. Southern drawl")
    , ("jethro",     "jethro <phrase>. Now listen to a story 'bout a man named Jed...")
    , ("jive",       "jive <phrase>. Slap ma fro")
    , ("kraut",      "kraut <phrase>. German accent")
    , ("pansy",      "pansy <phrase>. Effeminate male")
    , ("pirate",     "pirate <phrase>. Talk like a pirate")
    , ("postmodern", "postmodern <phrase>. Feminazi")
    , ("redneck",    "redneck <phrase>. Deep south")
    , ("valspeak",   "valley <phrase>. Like, ya know?")
    , ("warez",      "warez <phrase>. H4x0r")
    ]

pathTo :: String -> String
pathTo f = "/usr/local/bin/" ++ f

runFilter :: String -> String -> IO [String]
runFilter f s = do
        (out,_,_) <- popen (pathTo f) [] (Just s)
        return $ result out
    where result [] = ["Couldn't run the filter."]
          result xs = filter (not . all (==' ')) . lines $ xs
