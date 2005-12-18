--
-- | 
-- Module    : Where
-- Copyright : 2003 Shae Erisson
--
-- License:     lGPL
--
-- Slightly specialised version of Where for associating projects with their urls.
-- Code almost all copied.
--
module Plugins.Where (theModule) where

import Lambdabot
import LBState
import Util
import Serial
import qualified Data.FastPackedString as P
import qualified Map as M

------------------------------------------------------------------------

newtype WhereModule = WhereModule ()

theModule :: MODULE
theModule = MODULE $ WhereModule()

type WhereState         = M.Map P.FastString P.FastString
type WhereWriter        = WhereState -> LB ()
type Where m a          = ModuleT WhereState m a

instance Module WhereModule WhereState where

  moduleCmds _ = ["where", "what", "where+" ]
  moduleHelp _ s = case s of
    "where"    -> "@where <key>, return element associated with key"
    "where+"   -> "@where+ <key> <elem>, define an association"
    _          -> "Remember urls of open source projects"

  moduleDefState  _ = return $ M.empty
  moduleSerialize _ = Just mapPackedSerial

  process_ _ cmd rest = do
        result <- withMS $ \factFM writer -> case words rest of
            []         -> return "@where <key>, return element associated with key"
            (fact:dat) -> processCommand factFM writer
                                (lowerCaseString fact) cmd (unwords dat)
        return [result]

------------------------------------------------------------------------

processCommand :: WhereState -> WhereWriter
               -> String -> String -> String -> Where LB String

processCommand factFM writer fact cmd dat = case cmd of
        "where"     -> return $ getWhere factFM fact
        "what"      -> return $ getWhere factFM fact -- an alias
        "where+"    -> updateWhere True factFM writer fact dat
        _           -> return "Unknown command."

getWhere :: WhereState -> String -> String
getWhere fm fact =
    case M.lookup (P.pack fact) fm of
        Nothing -> "I know nothing about " ++ fact ++ "."
        Just x  -> P.unpack x

updateWhere :: Bool -> WhereState -> WhereWriter -> String -> String -> Where LB String
updateWhere _guard factFM writer fact dat = do 
        writer $ M.insert (P.pack fact) (P.pack dat) factFM
        return "Done."

