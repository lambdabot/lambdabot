{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}
--
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- grab a random line of code from a directory full of .hs code
--

module Plugin.Code where

import Plugin
import Text.Regex

$(plugin "Code")

instance Module CodeModule [FilePath] where

  moduleDefState _ = io $ getSourceFiles $
        fptoolsPath config </> "libraries" </> "base"

  moduleHelp _ _ = "code. Print random line of code from $fptools"
  moduleCmds   _ = ["code"]

  process_ _ "code" _ = do
        fs <- readMS
        (file,line) <- liftIO $ do
                    f    <- stdGetRandItem fs
                    h    <- openFile f ReadMode
                    s    <- hGetContents h
                    l    <- getRandSrcOf (lines s) 1000 -- number of times to try
                    hClose h
                    return (f, (dropSpace . expandTab $ l))

        -- dump raw output
        return [basename file ++ ": " ++ line]

--
-- work out our list of potential source files
-- evil!
--
-- Going to be expensive at startup, no?
--
getSourceFiles :: FilePath -> IO [FilePath]
getSourceFiles d = do
        (o,_,_) <- popen "/usr/bin/find" [d,"-name","*.hs","-o","-name","*.lhs"] Nothing
        return (lines o)

-- give up:
getRandSrcOf :: [String] -> Int -> IO String
getRandSrcOf s 0 | s == []   = return []
                 | otherwise = return $ head s

-- otherwise get a random src line
getRandSrcOf ss n = do
        s <- stdGetRandItem ss
        case () of {_
                | Just _ <- comment `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- ws      `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- nl      `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- pragma  `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- imports `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- wheres  `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- mods    `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- nested  `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- cpp     `matchRegex` s -> getRandSrcOf ss (n-1)
                | Just _ <- cpp'    `matchRegex` s -> getRandSrcOf ss (n-1)
                | length s < 30                    -> getRandSrcOf ss (n-1)
                | otherwise                        -> return s -- got it
        }
        where comment = mkRegex "--"
              nested  = mkRegex "{"
              pragma  = mkRegex "OPTION"
              cpp     = mkRegex "#if"
              cpp'    = mkRegex "#include"
              ws      = mkRegex "^ "	-- line *must* start with an identifer
              nl      = mkRegex "\n"
              imports = mkRegex "^import"
              wheres  = mkRegex "^ *where"
              mods    = mkRegex "module"

