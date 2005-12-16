--
-- Copyright (c) 2005 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
-- 
-- Written: Mon Dec 12 10:16:56 EST 2005
--

--
-- | A binding to Djinn.
--
module Plugins.Djinn (theModule) where

-- import Serial
import LBState
import Lambdabot      hiding  (clean)
import Util           hiding  (clean)
import PosixCompat

import System.IO

import Data.List                (intersperse, nub)
import Data.Either

import Control.Monad.Trans      (liftIO)
import Text.Regex

------------------------------------------------------------------------

newtype DjinnModule = DjinnModule ()

theModule :: MODULE
theModule = MODULE $ DjinnModule ()

-- | We can accumulate an interesting environment
type DjinnEnv = ([Decl] {- prelude -}, [Decl])
type Decl = String

------------------------------------------------------------------------

instance Module DjinnModule DjinnEnv where

        moduleHelp _ s = case s of
            "djinn"     -> "Generates Haskell code from a type.\n" ++
                           "http://darcs.augustsson.net/Darcs/Djinn"
            "djinn-add" -> "Define a new function type or type synonym"
            "djinn-del" -> "Remove a symbol from the environment"
            "djinn-clr" -> "Reset the djinn environment"
            "djinn-env" -> "Show the current djinn environment"
            "djinn-ver" -> "Show current djinn version"
            _           -> error "invalid command to Djinn.moduleHelp"

        moduleCmds      _ = ["djinn"
                            ,"djinn-add"
                            ,"djinn-del"
                            ,"djinn-env"
                            ,"djinn-clr" 
                            ,"djinn-ver"]

        moduleSerialize _ = Nothing -- Just listSerial

        moduleDefState  _ = do
            st <- liftIO $ getDjinnEnv ([],[]) -- get the prelude
            return (either (const []) snd{-!-} st, [])

        -- rule out attempts to do IO, if these get into the env,
        -- they'll be executed by djinn
        process _ _ src _ s | Just _ <- cmd  `matchRegex` s = end
          where end  = ircPrivmsg src "Invalid command"
                cmd  = mkRegex "^ *:"

        -- Normal commands
        process _ _ src "djinn" s = do
                (_,env) <- readMS
                e       <- liftIO $ djinn env $ ":set +sorted" <$> "f ?" <+> s
                mapM_ (ircPrivmsg src) $ either id (tail . lines) e

        -- Augment environment. Have it checked by djinn.
        process _ _ src "djinn-add"  s = do
            (p,st)  <- readMS 
            est     <- liftIO $ getDjinnEnv $ (p, dropSpace s : st)
            case est of
                Left e     -> ircPrivmsg src (head e)
                Right st'' -> modifyMS $ const st''

        -- Display the environment
        process _ _ src "djinn-env"  _ = do
            (prelude,st) <- readMS
            mapM_ (ircPrivmsg src) (prelude ++ st)

        -- Reset the env
        process _ _ _ "djinn-clr"  _ = modifyMS $ \(p,_) -> (p,[])

        -- Remove sym from environment. We let djinn do the hard work of
        -- looking up the symbols.
        process _ _ src "djinn-del" s =  do
            (_,env) <- readMS
            eenv <- liftIO $ djinn env $ ":delete" <+> dropSpace s <$> ":environment"
            case eenv of
                Left e     -> ircPrivmsg src (head e)
                Right env' -> modifyMS $ \(prel,_) ->
                    (prel,filter (\p -> p `notElem` prel) . nub . lines $ env')

        -- Version number
        process _ _ src "djinn-ver"  _ = do
            (out,_,_) <- liftIO $ popen binary [] (Just ":q")
            let v = dropNL . clean . drop 18 . head . lines $ out
            ircPrivmsg src v

        process _ _ _ _ _ = error "DjinnModule: invalid command"

------------------------------------------------------------------------

-- | Should be built inplace by the build system
binary :: String
binary = "./djinn"

-- | Extract the default environment
getDjinnEnv :: DjinnEnv -> IO (Either [String] DjinnEnv)
getDjinnEnv (prel,env') = do
    env <- djinn env' ":environment"
    case env of
        Left e  -> return $ Left e
        Right o -> do let new = filter (\p -> p `notElem` prel) . nub .  lines $ o
                      return $ Right (prel, new)

-- | Call the binary:

djinn :: [Decl] -> String -> IO (Either [String] String)
djinn env' src = do
    let env = concat . intersperse "\n" $ env'
    (out,_,_) <- popen binary [] (Just (env <$> src <$> ":q"))
    let o = dropNL . clean . unlines . init . drop 2 . lines $ out
    return $ case () of {_
        | Just _ <- failed `matchRegexAll` o -> Left (lines o)
        | Just _ <- unify  `matchRegexAll` o -> Left (lines o)
        | otherwise                          -> Right o
    }
    where
        failed = mkRegex "Cannot parse command"
        unify  = mkRegex "cannot be realized"

--
-- Clean up djinn output
--
clean :: String -> String
clean s | Just (a,_,b,_) <- prompt `matchRegexAll` s = a ++ clean b
        | otherwise      = s
    where
        prompt = mkRegex "Djinn>[^\n]*\n"

