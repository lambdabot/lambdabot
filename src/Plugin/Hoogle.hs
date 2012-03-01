{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- | Talk to Neil Mitchell's `Hoogle' program
module Plugin.Hoogle (theModule) where

import Plugin

$(plugin "Hoogle")

type HoogleState = [String]

instance Module HoogleModule where
    type ModuleState HoogleModule = HoogleState
    
    moduleDefState _ = return []
    moduleCmds _ = 
        [ (command "hoogle")
            { help = say "hoogle <expr>. Haskell API Search for either names, or types."
            , process = \s -> do
                o <- io (hoogle s)
                let (this,that) = splitAt 3 o
                lift (writeMS that)
                mapM_ say this
            }
        , (command "hoogle+")
            -- TODO: what does this really do?  give it a proper help msg
            { help = say "hoogle <expr>. Haskell API Search for either names, or types."
            , process = \s -> do
                this <- lift $ withMS $ \st write -> do
                                let (this,that) = splitAt 3 st
                                write that
                                return this
                mapM_ say this
            }
        ]

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
