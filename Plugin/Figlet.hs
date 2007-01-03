--
-- Copyright (c) 2006 Maxime Henrion <mux@FreeBSD.org>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

--
-- Call the `figlet' program
--

module Plugin.Figlet where

import Plugin

PLUGIN Figlet

instance Module FigletModule () where
    moduleCmds _ = ["figlet", "figlet'"]
    moduleHelp _ = fromJust . flip lookup help

    process_ _ "figlet" s = do
        let usage = ["usage: figlet <text>."]
        case words s of
          [] -> return usage
          t -> io (figlet (unwords t) Nothing)

    process_ _ "figlet'" s = do
        let usage = ["usage: figlet' <font> <text>."]
        case words s of
          (f:t@(_:_)) -> io (figlet (unwords t) (Just f))
          _ -> return usage

-- | Lookup table for documentation
help :: [(String, String)]
help =
    [("figlet", "figlet <text>. Run the figlet filter on <text>."),
     ("figlet'", "figlet' <font> <text>. Like figlet but using font <font>.")]

figletBinary :: FilePath
figletBinary = "/usr/local/bin/figlet"

-- | Run the actual figlet command and clean output
figlet :: String -> Maybe String -> IO [String]
figlet s f = do
        let args = ["-w", "70"          -- Limit width to 70 characters
                   ,"-f", fromMaybe "standard" f
                   ,s]
        (out,_,_) <- popen figletBinary args Nothing
        return $ result out

    where result [] = ["Couldn't run the figlet command."]
          result xs = filter (not . all (==' ')) . lines $ xs
