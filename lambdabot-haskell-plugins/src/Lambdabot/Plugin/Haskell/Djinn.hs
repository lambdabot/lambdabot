{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) 2005 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- Written: Mon Dec 12 10:16:56 EST 2005

-- | A binding to Djinn.
module Lambdabot.Plugin.Haskell.Djinn (djinnPlugin) where

import Lambdabot.Config.Haskell
import Lambdabot.Logging
import Lambdabot.Plugin
import Lambdabot.Util

import Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Maybe
import System.Process (readProcess)
import Text.Regex.TDFA

-- | We can accumulate an interesting environment
type DjinnEnv = ([Decl] {- prelude -}, [Decl])
type Djinn = ModuleT (Maybe DjinnEnv) LB
type Decl = String

djinnPlugin :: Module (Maybe DjinnEnv)
djinnPlugin = newModule
    { moduleSerialize = Nothing
    , moduleDefState = return Nothing
    
    -- gratuitous invocation at startup to let the user know if the command is missing
    , moduleInit = void (djinn [] "")

    , moduleCmds = return
        [ (command "djinn")
            { help = mapM_ say
                [ "djinn <type>."
                , "Generates Haskell code from a type."
                , "http://darcs.augustsson.net/Darcs/Djinn"
                ]
            , process = rejectingCmds djinnCmd
            }
        , (command "djinn-add")
            { help = do
                say "djinn-add <expr>."
                say "Define a new function type or type synonym"
            , process = rejectingCmds djinnAddCmd
            }
        , (command "djinn-del")
            { help = do
                say "djinn-del <ident>."
                say "Remove a symbol from the environment"
            , process = rejectingCmds djinnDelCmd
            }
        , (command "djinn-env")
            { help = do
                say "djinn-env."
                say "Show the current djinn environment"
            , process = const djinnEnvCmd
            }
        , (command "djinn-names")
            { help = do
                say "djinn-names."
                say "Show the current djinn environment, compactly."
            , process = const djinnNamesCmd
            }
        , (command "djinn-clr")
            { help = do
                say "djinn-clr."
                say "Reset the djinn environment"
            , process = const djinnClrCmd
            }
        , (command "djinn-ver")
            { help = do
                say "djinn-ver."
                say "Show current djinn version"
            , process = const djinnVerCmd
            }
        ]
    }

getSavedEnv :: Djinn DjinnEnv
getSavedEnv = withMS $ \st write -> 
    case st of
        Just env -> return env
        Nothing -> do
            st' <- getDjinnEnv ([],[]) -- get the prelude
            
            -- TODO: don't swallow errors here
            let newMS = (either (const []) snd{-!-} st', [])
            write (Just newMS)
            return newMS

getUserEnv :: Djinn [Decl]
getUserEnv = fmap snd getSavedEnv

-- check the args, reject them if they start with a colon (ignoring whitespace)
rejectingCmds :: Monad m => ([Char] -> Cmd m ()) -> [Char] -> Cmd m ()
rejectingCmds action args
    | take 1 (dropWhile isSpace args) == ":"
                = say "Invalid command"
    | otherwise = action args

-- Normal commands
djinnCmd :: [Char] -> Cmd Djinn ()
djinnCmd s = do
        env     <- lift getUserEnv
        e       <- djinn env $ ":set +sorted\nf ? " ++ dropForall s
        mapM_ say $ either id (parse . lines) e
    where
      dropForall t = maybe t mrAfter (t =~~ re)
      re = "^forall [[:alnum:][:space:]]+\\."
      parse :: [String] -> [String]
      parse x = if length x < 2
                then ["No output from Djinn; installed?"]
                else tail x

-- Augment environment. Have it checked by djinn.
djinnAddCmd :: [Char] -> Cmd Djinn ()
djinnAddCmd s = do
    (p,st)  <- lift getSavedEnv
    est     <- getDjinnEnv (p, strip isSpace s : st)
    case est of
        Left e     -> say (head e)
        Right st'  -> writeMS (Just st')

-- Display the environment
djinnEnvCmd :: Cmd Djinn ()
djinnEnvCmd = do
    (prelude,st) <- lift getSavedEnv
    mapM_ say $ prelude ++ st

-- Display the environment's names (quarter-baked)
djinnNamesCmd :: Cmd Djinn ()
djinnNamesCmd = do
    (prelude,st) <- lift getSavedEnv
    let names = concat $ intersperse " " $ concatMap extractNames $ prelude ++ st
    say names
  where extractNames = filter (isUpper . head) . unfoldr (\x -> case x of _:_ -> listToMaybe (lex x); _ -> Nothing)

-- Reset the env
djinnClrCmd :: Cmd Djinn ()
djinnClrCmd = writeMS Nothing

-- Remove sym from environment. We let djinn do the hard work of
-- looking up the symbols.
djinnDelCmd :: [Char] -> Cmd Djinn ()
djinnDelCmd s =  do
    (_,env) <- lift getSavedEnv
    eenv <- djinn env $ ":delete " ++ strip isSpace s ++ "\n:environment"
    case eenv of
        Left e     -> say (head e)
        Right env' -> modifyMS . fmap $ \(prel,_) ->
            (prel,filter (`notElem` prel) . nub . lines $ env')

-- Version number
djinnVerCmd :: Cmd Djinn ()
djinnVerCmd = say =<< getDjinnVersion

------------------------------------------------------------------------

-- | Extract the default environment
getDjinnEnv :: (MonadLB m) => DjinnEnv -> m (Either [String] DjinnEnv)
getDjinnEnv (prel,env') = do
    env <- djinn env' ":environment"
    return (either Left (Right . readEnv) env)
    where
        readEnv o = let new = filter (\p -> p `notElem` prel) . nub .  lines $ o
                     in (prel, new)

getDjinnVersion :: MonadLB m => m String
getDjinnVersion = do
    binary <- getConfig djinnBinary
    io (fmap readVersion (readProcess binary [] ":q"))
        `E.catch` \SomeException{} ->
            return "The djinn command does not appear to be installed."
    where 
        readVersion = extractVersion . unlines . take 1 . lines
        extractVersion str = case str =~~ "version [0-9]+(-[0-9]+)*" of
            Nothing -> "Unknown"
            Just m  -> m

-- | Call the binary:

djinn :: MonadLB m => [Decl] -> String -> m (Either [String] String)
djinn env src = do
    binary <- getConfig djinnBinary
    io (tryDjinn binary env src)
        `E.catch` \e@SomeException{} -> do
            let cmdDesc = case binary of
                    "djinn" -> ""
                    _       -> "(" ++ binary ++ ") "
                msg = "Djinn command " ++ cmdDesc ++ "failed: " ++ show e
            errorM msg
            return (Left [msg])

tryDjinn :: String -> [Decl] -> String -> IO (Either [String] String)
tryDjinn binary env src = do
    out <- readProcess binary [] (unlines (env ++ [src, ":q"]))
    let safeInit [] = []
        safeInit xs = init xs
        o = dropFromEnd (== '\n') . clean_ . unlines . safeInit . drop 2 . lines $ out
    return $ case () of {_
        | o =~ "Cannot parse command" ||
          o =~ "cannot be realized"   ||
          o =~ "^Error:"                -> Left (lines o)
        | otherwise                     -> Right o
    }

--
-- Clean up djinn output
--
clean_ :: String -> String
clean_ s | Just mr <- s =~~ prompt  = mrBefore mr ++ mrAfter mr
         | otherwise                = s
    where
        prompt = "(Djinn> *)+"
