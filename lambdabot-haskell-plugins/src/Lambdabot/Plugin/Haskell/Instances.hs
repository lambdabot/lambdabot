{- | A module to output the instances of a typeclass.
     Some sample input\/output:

> lambdabot> @instances Monad
> [], ArrowMonad a, WriterT w m, Writer w, ReaderT r m, Reader r,
> StateT s m, State s, RWST r w s m, RWS r w s, ErrorT e m, Either e,
> ContT r m, Cont r, Maybe, ST s, IO
>
> lambdabot> @instances Show
> Float, Double, Integer, ST s a, [a], (a, b, c, d), (a, b, c), (a, b),
> (), Ordering, Maybe a, Int, Either a b, Char, Bool
>
> lambdabot> @instances-importing Text.Html Data.Tree Show
> Float, Double, Tree a, HtmlTable, HtmlAttr, Html, HotLink, Integer,
> ST s a, [a], (a, b, c, d), (a, b, c), (a, b), (), Ordering, Maybe a,
> Int
-}

module Lambdabot.Plugin.Haskell.Instances (instancesPlugin) where

import Text.ParserCombinators.Parsec

import Lambdabot.Config.Haskell
import Lambdabot.Plugin
import Lambdabot.Util
import Lambdabot.Plugin.Haskell.Eval (findL_hs)

import Control.Applicative ((*>))
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.FilePath
import System.Process
import Text.Regex.TDFA

type Instance   = String
type ClassName  = String
type ModuleName = String

instancesPlugin :: Module ()
instancesPlugin = newModule
    { moduleCmds = return
        [ (command "instances")
            { help = say "instances <typeclass>. Fetch the instances of a typeclass."
            , process = fetchInstances >=> say
            }
        , (command "instances-importing")
            { help = say $
                "instances-importing [<module> [<module> [<module...]]] <typeclass>. " ++
                "Fetch the instances of a typeclass, importing specified modules first."
            , process = fetchInstancesImporting >=> say
            }
        ]
    }

-- | Nice little combinator used to throw away error messages from an Either
--   and just keep a Maybe indicating the success of the computation.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- * Parsing
--

-- | Parse an instance declaration. Sample inputs:
--
-- > instance Monad []
-- > instance (Monoid w) => Monad (Writer w)
-- > instance (State s)
--
instanceP :: ClassName -> CharParser st Instance
instanceP cls
    =  string "instance " *> (try constrained <|> unconstrained) *> skipMany space
    *> anyChar `manyTill` end
    where constrained   = noneOf "=" `manyTill` string ("=> " ++ cls)
          unconstrained = string cls
          
          -- break on the "imported from" comment or a newline.
          end           = void (try (string "--")) <|> eof

-- | Wrapper for the instance parser.
parseInstance :: ClassName -> String -> Maybe Instance
parseInstance cls = fmap (strip isSpace) . eitherToMaybe
                    . parse (instanceP cls) "GHCi output"

-- | Split the input into a list of the instances, then run each instance
--   through the parser. Collect successes.
getInstances :: String -> ClassName -> [Instance]
getInstances s cls
    | not classFound -- can't trust those dodgy folk in #haskell
    = ["Couldn't find class `"++cls++"'. Try @instances-importing"]

   | otherwise = sort $ mapMaybe doParse (tail splut)

    where classFound   = s =~ ("class.*" ++ cls ++ ".*where")
          splut        = splitOn "instance" s -- splut being the past participle
                                            -- of 'to split', obviously. :)
          notOperator  = all (\c -> or
                               [ isAlpha c,
                                 isSpace c,
                                 c `elem` "()" ])
          unbracket str | head str == '(' && last str == ')' &&
                          all (/=',') str && notOperator str && str /= "()" =
                          init $ tail str
                        | otherwise = str
          doParse = fmap unbracket . parseInstance cls . ("instance"++)

-- * Delegation; interface with GHCi
--

-- | The standard modules we ask GHCi to load.
stdMdls :: [ModuleName]
stdMdls = controls
    where monads   = map ("Monad."++)
                       [ "Cont", "Error", "Fix", "Reader", "RWS", "ST",
                         "State", "Trans", "Writer" ]
          controls = map ("Control." ++) $ monads ++ ["Arrow"]

-- | Main processing function for \@instances. Takes a class name and
--   return a list of lines to output (which will actually only be one).
fetchInstances :: MonadLB m => ClassName -> m String
fetchInstances cls = fetchInstances' cls stdMdls

-- | Main processing function for \@instances-importing. Takes the args, which
--   are words'd. The all but the last argument are taken to be the modules to
--   import, and the last is the typeclass whose instances we want to print.
fetchInstancesImporting :: MonadLB m => String -> m String
fetchInstancesImporting args = fetchInstances' cls mdls
    where args' = words args
          cls   = last args'
          mdls  = nub $ init args' ++ stdMdls

-- | Interface with GHCi to get the input for the parser, then send it through
--   the parser.
fetchInstances' :: MonadLB m => String -> [ModuleName] -> m String
fetchInstances' cls mdls = do
    load <- findL_hs
    let s = unlines $ map unwords
            [ [":l", load]
            ,  ":m" : "+" : mdls
            , [":i", cls]
            ]

    ghci <- getConfig ghciBinary
    (_, out, err) <- io $ readProcessWithExitCode ghci ["-ignore-dot-ghci","-fglasgow-exts"] s
    let is = getInstances out cls
    return $ if null is
               then err
               else intercalate ", " is
