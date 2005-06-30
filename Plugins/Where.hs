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
import qualified Map as M

------------------------------------------------------------------------

newtype WhereModule = WhereModule ()

theModule :: MODULE
theModule = MODULE $ WhereModule()

type WhereState         = M.Map String String
type WhereWriter        = WhereState -> LB ()
type Where m a          = ModuleT WhereState m a

instance Module WhereModule WhereState where

  moduleHelp _ s = return $ case s of
    "where"    -> "@where <key>, return element associated with key"
    "where+"   -> "@where+ <key> <elem>, define an association"
    _          -> "Remember urls of open source projects"

  moduleDefState _  = return $ M.empty
  moduleSerialize _ = Just mapSerializer
  moduleCmds   _ = return ["where", "where+" ]

  process _ _ target cmd rest = do
        result <- withMS $ \factFM writer -> case words rest of
            []         -> return "I can not handle empty facts."
            (fact:dat) -> processCommand factFM writer
                                (lowerCaseString fact) cmd (unwords dat)
        ircPrivmsg target result

------------------------------------------------------------------------

processCommand :: WhereState -> WhereWriter
               -> String -> String -> String -> Where LB String
processCommand factFM writer fact cmd dat = case cmd of
        "where"     -> return $ getWhere factFM fact
        "where+"    -> updateWhere True factFM writer fact dat
        _           -> return "Unknown command."

getWhere :: M.Map String String -> String -> String
getWhere fm fact =
    case M.lookup fact fm of
        Nothing -> "I know nothing about " ++ fact ++ "."
        Just x  -> fact ++ ": " ++ x ++ "."

updateWhere :: Bool -> WhereState -> WhereWriter -> String -> String -> Where LB String
updateWhere _guard factFM writer fact dat = do 
        writer $ M.insert fact dat factFM
        return $ fact ++ " ~> " ++ dat 
