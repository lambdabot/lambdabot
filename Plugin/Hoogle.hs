{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}

-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Talk to Neil Mitchell's `Hoogle' program
module Plugin.Hoogle (theModule) where

import Plugin

$(plugin "Hoogle")

type HoogleState = [String]

instance Module HoogleModule HoogleState where
    moduleDefState _ = return []
    moduleCmds   _ = ["hoogle", "hoogle+"]
    moduleHelp _ _ = "hoogle <expr>. Haskell API Search for either names, or types."

    process_ _ "hoogle" s = do
        o <- io (hoogle s)
        let (this,that) = splitAt 3 o
        writeMS that
        return [unlines this]

    process_ _ "hoogle+" _ = do
        this <- withMS $ \st write -> do
                        let (this,that) = splitAt 3 st
                        write that
                        return this
        return [unlines this]

------------------------------------------------------------------------

hoogleBinary :: FilePath
hoogleBinary = "hoogle"

-- arbitrary cutoff point
cutoff :: Int
cutoff = -10

-- | Actually run the hoogle binary
hoogle :: String -> IO [String]
hoogle s = do
        let args = ["--count=20", s]
        (out,err,_) <- popen hoogleBinary args (Just "")
        return $ result out err

    where result [] [] = ["A Hoogle error occurred."]
          result [] ys = [ys]
          result xs _  =
                let xs' = map toPair $ lines xs
                    res = map snd $ filter ((>=cutoff) . fst) xs'
                in if null res
                   then ["No matches, try a more general search"]
                   else res

          toPair s' = let (res, meta)  = break (=='@') s'
                          rank = takeWhile (/=' ') . drop 2 $ meta
                      in case readM rank :: Maybe Int of
                         Just n  -> (n,res)
                         Nothing -> (0,res)
