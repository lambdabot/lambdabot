-- Copyright 2006, Sascha Boehme.




-- Main module of the application.

module Main (main) where



import FreeTheorems
import PredefinedTypes

import Control.Monad
import Data.List
import System.Environment
import System.Console.GetOpt
import System.IO
import Text.ParserCombinators.Parsec (ParseError)

computeResult' :: String -> String
computeResult' t =
    let model = FixModel
        omitIns = True
    in
      case parseTypeString t of
        Left error -> "There was an error in the type: "++show error
        Right nt   ->
          let (th, td) = generateTheorem model nt
              (th2, td2) = foldl instantiateRelation (th, td)
                                 (extractRelationVariables th)
              tm2 = if omitIns then omitInstantiations th2 else th2
              ls2 = extractLiftRelations (th2, td2) omitIns
          in  (printAsText tm2)
              ++ (concatMap (\l -> "\n\n"++(printAsText l)) ls2)

main = putStr . computeResult' =<< getLine


-- Entry point for the application.
-- The function parses the command line arguments. Then, it starts the
-- computation with the given values.

{-
main :: IO ()
main = do
  arguments <- getArgs
  case getOpt Permute options arguments of
    (opts, _, [])  -> let (info, os) = checkOpt opts
                      in  if info
                            then putStrLn showInfo
                            else case os of
                                   Left errors   -> showErrors errors
                                   Right (t,m,o) -> putStrLn
                                                    $ computeResult t m o
    (_, _, errors) -> showErrors errors

  where
    showErrors errors = do
      mapM (\s -> hPutStr stderr ("Error: " ++ s)) errors
      hPutStr stderr (usageInfo "Usage: ftbase [OPTIONS]" options)

    showInfo =
        xmlTag "info" [] (concat
          [ concatMap (\(n,i) -> xmlData n i) getSupportedAlgebraicDatatypes
          , concatMap (\(n,i) -> xmlType n i) getSupportedTypeSynonyms
          , concatMap (\(n,t) -> xmlPred n t) predefinedTypes
          ])
      where
        xmlData n i = xmlEmptyTag "data" [("name", n), ("arity", show i)]
        xmlType n i = xmlEmptyTag "type" [("name", n), ("arity", show i)]
        xmlPred n t = xmlTag "predefined" [("name", n)] t

-}



--------------------------------------------------------------------------------
--
-- Parsing of command line arguments.
--



-- Abstract description of command line options.

data Flag = Type String
          | Model LanguageModel
          | Info
          | OmitInstantiations
          deriving (Eq, Show)



-- The command line options.

options :: [OptDescr Flag]
options =
  [ Option ['t'] ["type"]
           (ReqArg Type "TYPE")
           "the type from which to generate a theorem"

  , Option ['b'] ["basic-model"]
           (NoArg (Model BasicModel))
           "use the basic model"

  , Option ['f'] ["fix-model"]
           (NoArg (Model FixModel))
           "use the fix model"

  , Option ['o'] ["omit-instantiations"]
           (NoArg OmitInstantiations)
           "omit instantiations in the generated theorem"

  , Option ['i'] ["info"]
           (NoArg Info)
           "show available constructors and predefined types"
  ]



-- Checks if the detected command line options are correct, i.e. if flags of
-- each kind are occurring only once. If so, the corresponding values are
-- returned.

checkOpt :: [Flag] -> (Bool, Either [String] (LanguageModel, String, Bool))
checkOpt flags =
  let om = foldr (checkModel) (Left "missing model\n") flags
      ot = case foldr (checkType) (Left "missing type\n") flags of
             Left e  -> Left e
             Right t -> if length (words t) > 0
                          then Right t
                          else Left "missing type\n"
      oi = any (\f -> f == Info) flags
      oo = any (\f -> f == OmitInstantiations) flags

      extractError = either (\e -> [e]) (\_ -> [])
      errors = extractError om ++ extractError ot

  in  case (om,ot) of
        (Right m, Right t) -> (oi, Right (m, t, oo))
        otherwise          -> (oi, Left errors)

  where
    checkModel (Model m) (Left _)  = Right m
    checkModel (Model _) (Right _) = Left "only one model can be given\n"
    checkModel _         x         = x

    checkType (Type t) (Left _)  = Right t
    checkType (Type _) (Right _) = Left "only one type can be given\n"
    checkType _        x         = x





--------------------------------------------------------------------------------
--
-- Computation of output
--



-- Parses the given string and creates either an error message or valid output.

computeResult :: LanguageModel -> String -> Bool -> String
computeResult model t omitIns =
  case parseTypeString t of
    Left error -> xmlTag "error" [("type", t)] (xmlClean (show error))
    Right nt   ->
      let (th, td) = generateTheorem model nt
          tm = if omitIns then omitInstantiations th else th
          ls = extractLiftRelations (th, td) omitIns

          (th2, td2) = foldl instantiateRelation (th, td)
                             (extractRelationVariables th)
          tm2 = if omitIns then omitInstantiations th2 else th2
          ls2 = extractLiftRelations (th2, td2) omitIns

      in  xmlTag "model" [] (show model)
          ++ xmlTag "type" [] (xmlClean (printAsText nt))
          ++ xmlTag "fundamental" [] (concat
               [ xmlTag "theorem"  [] (xmlClean (printAsText tm))
               , concatMap (\l -> xmlTag "lift"  [] (xmlClean (printAsText l))) ls
               ])
          ++ xmlTag "reduced" [] (concat
               [ xmlTag "theorem"  [] (xmlClean (printAsText tm2))
               , concatMap (\l -> xmlTag "lift"  [] (xmlClean (printAsText l))) ls2
               ])



-- Parses a type string and returns the named type obtained from parsing or an
-- error if no type was found. The function works as follows:
--
--   * If the input contains "::", then it is parsed as a named type.
--
--   * If the input is only one word, the function tries to find a type with
--     that name in the list of predefined types. If it can be found, that type
--     is parsed as a named type.
--
--   * Otherwise the input is parsed as a type without name, while a new name is
--     generated.

parseTypeString :: String -> Either ParseError NamedType
parseTypeString string =
  let wlist = words string
  in if (containsTwoColons string)
       then parseNamedType string
       else let (w:ws) = wlist
            in  if (ws == [])
                  then case lookup w predefinedTypes of
                         Nothing -> parseType acceptName string
                         Just t  -> parseNamedType (w ++ " :: " ++ t)
                  else parseType acceptName string
  where
    containsTwoColons str =
      case str of
        ':' : ':' : cs -> True
        _   :  c  : cs -> containsTwoColons (c:cs)
        otherwise     -> False

    acceptName name =
      let (predefinedNames, _) = unzip predefinedTypes
      in  name `notElem` predefinedNames





--------------------------------------------------------------------------------
--
-- XML functions
--



-- Creates an XML tag.

xmlTag :: String -> [(String, String)] -> String -> String
xmlTag n as c =
  "<" ++ n
  ++ foldl (\s (k,v) -> s ++ " " ++ k ++ "=\"" ++ xmlClean v ++ "\"") "" as
  ++ ">" ++ c ++ "</" ++ n ++ ">"



-- Creates an empty XML tag.

xmlEmptyTag :: String -> [(String, String)] -> String
xmlEmptyTag n as =
  "<" ++ n
  ++ foldl (\s (k,v) -> s ++ " " ++ k ++ "=\"" ++ xmlClean v ++ "\"") "" as
  ++ "/>"



-- Replaces XML special characters.

xmlClean :: String -> String
xmlClean = concatMap replaceSymbol
  where
    replaceSymbol '&' = "&amp;"
    replaceSymbol '>' = "&gt;"
    replaceSymbol '<' = "&lt;"
    replaceSymbol c   = [c]
