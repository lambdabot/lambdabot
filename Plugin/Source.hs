--
-- Plugin.Source
-- Display source for specified identifiers
--
module Plugin.Source (theModule) where

import Plugin
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (ByteString)
import System.Directory

PLUGIN Source

type Id  = ByteString
type Src = ByteString
type Env = M.Map Id Src

instance Module SourceModule Env where
    moduleCmds _     = ["src"]
    moduleHelp _ _   = "src <id>. Display the implementation of a standard function"

    -- pull the state file in (should all be automated, I guess)
    moduleDefState _ = return M.empty
    moduleInit _     = do
      b <- io $ doesFileExist file
      when b $ do
          s <- io $ do s <- P.readFile file
                       let m = M.fromList . map pair . splat . P.lines $ s
                       return m
          writeMS s

      where file = "State/source"

            splat [] = []
            splat s  = a : splat (tail b) where (a,b) = break P.null s

            pair (a:b) = (a, P.unlines b)

    process_ _ _ t = do
        env <- readMS
        return $ case M.lookup key env of
            _ | M.null env -> ["No source in the environment yet"]
            Nothing        -> ["Source for this function is not available."]
            Just s         -> [P.unpack s]
        where key = P.pack t

