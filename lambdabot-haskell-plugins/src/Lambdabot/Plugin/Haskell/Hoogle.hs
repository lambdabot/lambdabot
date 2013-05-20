-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Talk to Neil Mitchell's `Hoogle' program
module Lambdabot.Plugin.Haskell.Hoogle (hooglePlugin) where

import Lambdabot.Config.Haskell
import Lambdabot.Plugin
import Lambdabot.Util
import System.Process

hooglePlugin :: Module [String]
hooglePlugin = newModule
    { moduleDefState = return []
    , moduleCmds = return
        [ (command "hoogle")
            { help = say "hoogle <expr>. Haskell API Search for either names, or types."
            , process = \s -> do
                binary <- getConfig hoogleBinary
                o <- io (hoogle binary s)
                let (this,that) = splitAt 3 o
                writeMS that
                mapM_ say this
            }
        , (command "hoogle+")
            -- TODO: what does this really do?  give it a proper help msg
            { help = say "hoogle <expr>. Haskell API Search for either names, or types."
            , process = \_ -> do
                this <- withMS $ \st write -> do
                    let (this,that) = splitAt 3 st
                    write that
                    return this
                mapM_ say this
            }
        ]
    }

------------------------------------------------------------------------

-- arbitrary cutoff point
cutoff :: Int
cutoff = -10

-- | Actually run the hoogle binary
hoogle :: String -> String -> IO [String]
hoogle binary s = do
        let args = ["--count=20", s]
        (_,out,err) <- readProcessWithExitCode binary args ""
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
