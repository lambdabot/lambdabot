--
-- Copyright (c) 2005 Donald Bruce Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
-- 
-- Written: Mon Dec 12 10:16:56 EST 2005
--

--
-- | A binding to Djinn.
--
module Plugin.Djinn (theModule) where

import LBState
import Plugin

import Data.Either

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
            "djinn"     -> "djinn <type>.\nGenerates Haskell code from a type.\n" ++
                           "http://darcs.augustsson.net/Darcs/Djinn"
            "djinn-add" -> "djinn-add <expr>.\nDefine a new function type or type synonym"
            "djinn-del" -> "djinn-del <ident>.\nRemove a symbol from the environment"
            "djinn-clr" -> "djinn-clr.\nReset the djinn environment"
            "djinn-env" -> "djinn-env.\nShow the current djinn environment"
            "djinn-ver" -> "djinn-ver.\nShow current djinn version"

        moduleCmds      _ = ["djinn"
                            ,"djinn-add"
                            ,"djinn-del"
                            ,"djinn-env"
                            ,"djinn-clr" 
                            ,"djinn-ver"]

        moduleSerialize _ = Nothing -- Just listSerial

        moduleDefState  _ = do
            st <- io $ getDjinnEnv ([],[]) -- get the prelude
            return (either (const []) snd{-!-} st, [])

        -- rule out attempts to do IO, if these get into the env,
        -- they'll be executed by djinn
        process_ _ _ s | Just _ <- cmd  `matchRegex` s = end
          where end  = return ["Invalid command"]
                cmd  = mkRegex "^ *:"

        -- Normal commands
        process_ _ "djinn" s = do
                (_,env) <- readMS
                e       <- io $ djinn env $ ":set +sorted" <$> "f ?" <+> s
                return $ either id (tail . lines) e

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
    let o = dropNL . clean_ . unlines . init . drop 2 . lines $ out
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
clean_ :: String -> String
clean_ s | Just (a,_,b,_) <- prompt `matchRegexAll` s = a ++ clean_ b
        | otherwise      = s
    where
        prompt = mkRegex "Djinn>[^\n]*\n"

