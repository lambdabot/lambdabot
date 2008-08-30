{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- Module    : Fact
-- Copyright : 2003 Shae Erisson
-- Copyright : 2005-06 Don Stewart
--
-- License:     lGPL
--
-- Quick ugly hack to get factoids in lambdabot.  This is a rewrite of
-- Shae's original code to use internal module states. jlouis
module Plugin.Fact (theModule) where

import Plugin
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P

------------------------------------------------------------------------

$(plugin "Fact")

type FactState  = M.Map P.ByteString P.ByteString
type FactWriter = FactState -> LB ()
-- type Fact m a   = ModuleT FactState m a

instance Module FactModule FactState where

  moduleCmds   _ = ["fact","fact-set","fact-delete"
                   ,"fact-cons","fact-snoc","fact-update"]
  moduleHelp _ s = case s of
    "fact"        -> "fact <fact>, Retrieve a fact from the database"
    "fact-set"    -> "Define a new fact, guard if exists"
    "fact-update" -> "Define a new fact, overwriting"
    "fact-delete" -> "Delete a fact from the database"
    "fact-cons"   -> "cons information to fact"
    "fact-snoc"   -> "snoc information to fact"
    _             -> "Store and retrieve facts from a database"

  moduleDefState _  = return $ M.empty
  moduleSerialize _ = Just mapPackedSerial

  process_ _ cmd rest =
        list $ withMS $ \factFM writer -> case words rest of
            []         -> return "I can not handle empty facts."
            (fact:dat) -> processCommand factFM writer
                                (P.pack $ lowerCaseString fact)
                                cmd
                                (P.pack $ unwords dat)

------------------------------------------------------------------------

processCommand :: FactState -> FactWriter
               -> P.ByteString -> String -> P.ByteString -> LB String
processCommand factFM writer fact cmd dat = case cmd of
        "fact"        -> return $ getFact factFM fact
        "fact-set"    -> updateFact True factFM writer fact dat
        "fact-update" -> updateFact False factFM writer fact dat
        "fact-cons"   -> alterFact ((dat `P.append` (P.pack " ")) `P.append`) factFM writer fact
        "fact-snoc"   -> alterFact (P.append ((P.pack " ") `P.append` dat))   factFM writer fact
        "fact-delete" -> writer ( M.delete fact factFM ) >> return "Fact deleted."
        _ -> return "Unknown command."

updateFact :: Bool -> FactState -> FactWriter -> P.ByteString -> P.ByteString -> LB String
updateFact guarded factFM writer fact dat =
    if guarded && M.member fact factFM
        then return "Fact already exists, not updating"
        else writer ( M.insert fact dat factFM ) >> return "Fact recorded."

alterFact :: (P.ByteString -> P.ByteString)
          -> FactState -> FactWriter -> P.ByteString -> LB String
alterFact f factFM writer fact =
    case M.lookup fact factFM of
        Nothing -> return "A fact must exist to alter it"
        Just x  -> do writer $ M.insert fact (f x) factFM
                      return "Fact altered."

getFact :: M.Map P.ByteString P.ByteString -> P.ByteString -> String
getFact fm fact = case M.lookup fact fm of
        Nothing -> "I know nothing about " ++ P.unpack fact
        Just x  -> P.unpack fact ++ ": " ++ P.unpack x
