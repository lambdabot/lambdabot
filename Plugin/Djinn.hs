{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}
-- Copyright (c) 2005 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)

-- Written: Mon Dec 12 10:16:56 EST 2005

-- | A binding to Djinn.
module Plugin.Djinn (theModule) where

import Plugin
import Data.Char
import Data.List
import Data.Maybe

import qualified Text.Regex as R

$(plugin "Djinn")

-- | We can accumulate an interesting environment
type DjinnEnv = ([Decl] {- prelude -}, [Decl])
type Decl = String

instance Module DjinnModule DjinnEnv where

        moduleHelp _ s = case s of
            "djinn"     -> "djinn <type>.\nGenerates Haskell code from a type.\n" ++
                           "http://darcs.augustsson.net/Darcs/Djinn"
            "djinn-add" -> "djinn-add <expr>.\nDefine a new function type or type synonym"
            "djinn-del" -> "djinn-del <ident>.\nRemove a symbol from the environment"
            "djinn-clr" -> "djinn-clr.\nReset the djinn environment"
            "djinn-env" -> "djinn-env.\nShow the current djinn environment"
            "djinn-names" -> "djinn-names.\nShow the current djinn environment, compactly."
            "djinn-ver" -> "djinn-ver.\nShow current djinn version"

        moduleCmds      _ = ["djinn"
                            ,"djinn-add"
                            ,"djinn-del"
                            ,"djinn-env"
                            ,"djinn-names"
                            ,"djinn-clr"
                            ,"djinn-ver"]

        moduleSerialize _ = Nothing -- Just listSerial

        -- this means djinn better be visible at boot time
        moduleDefState  _ = do
                st <- io $ getDjinnEnv ([],[]) -- get the prelude
                return (either (const []) snd{-!-} st, [])

        -- rule out attempts to do IO, if these get into the env,
        -- they'll be executed by djinn
        process_ _ _ s | cmd  `matches'` s = end
          where end  = return ["Invalid command"]
                cmd  = regex' "^ *:"

        -- Normal commands
        process_ _ "djinn" s = do
                (_,env) <- readMS
                e       <- io $ djinn env $ ":set +sorted" <$> "f ?" <+> dropForall s
                return $ either id (parse . lines) e
            where
              dropForall t
                  | Just (_, _, x, _) <- R.matchRegexAll re t = x
                  | otherwise = t
              re = regex' "^forall [[:alnum:][:space:]]+\\."
              parse :: [String] -> [String]
              parse x = if length x < 2
                        then ["No output from Djinn; installed?"]
                        else tail x

        -- Augment environment. Have it checked by djinn.
        process_ _ "djinn-add"  s = do
            (p,st)  <- readMS
            est     <- io $ getDjinnEnv $ (p, dropSpace s : st)
            case est of
                Left e     -> return [head e]
                Right st'' -> modifyMS (const st'') >> return []

        -- Display the environment
        process_ _ "djinn-env"  _ = do
            (prelude,st) <- readMS
            return $ prelude ++ st

        -- Display the environment's names (quarter-baked)
        process_ _ "djinn-names"  _ = do
            (prelude,st) <- readMS
            let names = concat $ intersperse " " $ concatMap extractNames $ prelude ++ st
            return [names]
          where extractNames = filter (isUpper . head) . unfoldr (\x -> case x of _:_ -> listToMaybe (lex x); _ -> Nothing)

        -- Reset the env
        process_ _ "djinn-clr" _ = modifyMS (flip (,) [] . fst) >> return []

        -- Remove sym from environment. We let djinn do the hard work of
        -- looking up the symbols.
        process_ _ "djinn-del" s =  do
            (_,env) <- readMS
            eenv <- io $ djinn env $ ":delete" <+> dropSpace s <$> ":environment"
            case eenv of
                Left e     -> return [head e]
                Right env' -> do
                    modifyMS $ \(prel,_) ->
                        (prel,filter (\p -> p `notElem` prel) . nub . lines $ env')
                    return []

        -- Version number
        process_ _ "djinn-ver"  _ = do
            (out,_,_) <- io $ popen binary [] (Just ":q")
            let v = dropNL . clean_ . drop 18 . head . lines $ out
            return [v]

------------------------------------------------------------------------

-- | Should be built inplace by the build system
binary :: String
binary = "djinn"

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
    let safeInit [] = []
        safeInit xs = init xs
        o = dropNL . clean_ . unlines . safeInit . drop 2 . lines $ out
    return $ case () of {_
        | failed `matches'` o ||
          unify  `matches'` o ||
          err    `matches'` o -> Left (lines o)
        | otherwise                          -> Right o
    }
    where
        failed = regex' "Cannot parse command"
        unify  = regex' "cannot be realized"
        err = regex' "^Error:"

--
-- Clean up djinn output
--
clean_ :: String -> String
clean_ s | Just (a,_,b,_) <- prompt `R.matchRegexAll` s = a ++ clean_ b
         | otherwise      = s
    where prompt = regex' "Djinn>[^\n]*\n"

