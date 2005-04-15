--
-- | 
-- Module    : FactModule
-- Copyright : 2003 Shae Erisson
--
-- License:     lGPL
--
-- Quick ugly hack to get factoids in lambdabot.  This is a rewrite of
-- Shaes originial code to use internal module states. jlouis
--
module FactModule (theModule) where

import IRC
import Util
import qualified Map as M

------------------------------------------------------------------------

newtype FactModule = FactModule ()

theModule :: MODULE
theModule = MODULE $ FactModule()

type FactState = M.Map String String
type Fact m a = ModuleT FactState m a

instance Module FactModule FactState where

  moduleHelp _ s = return $ case s of
    "fact"        -> "@fact <fact>, Retrieve a fact from the database"
    "fact-set"    -> "Define a new fact, guard if exists"
    "fact-update" -> "Define a new fact, overwriting"
    "fact-delete" -> "Delete a fact from the database"
    "fact-cons"   -> "cons information to fact"
    "fact-snoc"   -> "snoc information to fact"
    _             -> "Store and retrieve facts from a database"

  moduleDefState _  = return $ M.empty
  moduleSerialize _ = Just mapSerializer
  moduleCmds   _ = return ["fact","fact-set","fact-delete",
                           "fact-cons","fact-snoc","fact-update"]

  process _ _ target cmd rest = do 
        factFM <- readMS
        result <- case words rest of
            []         -> return "I can not handle empty facts."
            (fact:dat) -> processCommand factFM 
                                (lowerCaseString fact) cmd (unwords dat)
        ircPrivmsg target result

------------------------------------------------------------------------

processCommand :: M.Map String String 
               -> String -> String -> String -> Fact IRC String
processCommand factFM fact cmd dat =
    case cmd of
        "fact"        -> return $ getFact factFM fact
        "fact-set"    -> updateFact True factFM fact dat
        "fact-update" -> updateFact False factFM fact dat
        "fact-cons"   -> alterFact ((dat ++ " ")++) factFM fact
        "fact-snoc"   -> alterFact (++(" " ++ dat)) factFM fact
        "fact-delete" -> do writeMS $ M.delete fact factFM
                            return "Fact deleted."
        _ -> return "Unknown command."

updateFact :: Bool -> M.Map String String -> String -> String -> Fact IRC String
updateFact guard factFM fact dat =
    if guard && M.member fact factFM
        then return "Fact is already existing, not updating"
        else do writeMS $ M.insert fact dat factFM
                return "Fact recorded."

alterFact :: (String -> String) 
          -> M.Map String String -> String -> Fact IRC String
alterFact f factFM fact =
    case M.lookup fact factFM of
        Nothing -> return "Fact must exist alter it"
        Just x  -> do writeMS $ M.insert fact (f x) factFM
                      return "Fact altered."

getFact :: M.Map String String -> String -> String
getFact fm fact =
    case M.lookup fact fm of
        Nothing -> "I know nothing about " ++ fact ++ "."
        Just x  -> fact ++ ": " ++ x ++ "."
