--
-- copyright (c) 2003 Shae Erisson
-- license: lGPL
-- quick ugly hack to get factoids in lambdabot
-- requires Database.PostgreSQL.HSQL from
-- http://sf.net/projects/htoolkit/
--
-- This is a rewrite of Shaes originial code to use internal module
-- states. jlouis@.
--

module FactModule (theModule) where

import IRC
import Util
import qualified Map as M

newtype FactModule = FactModule ()

theModule :: MODULE
theModule = MODULE $ FactModule()

type FactState = M.Map String String
-- type Fact m a = ModuleT FactState m a

instance Module FactModule FactState where
  moduleHelp _ "fact" = return "Retrieve a fact from the database"
  moduleHelp _ "fact-set" = return "Define a new fact"
  moduleHelp _ _ = return "Store and retrieve facts from a database"

  moduleDefState _  = return $ M.empty
  moduleSerialize _ = Just mapSerializer

  moduleCmds   _ = return ["fact", "fact-set"]

  process _ _ target cmd rest =
    do factFM <- readMS
       result <- case words rest of
                   [] -> return "I can not handle empty facts."
		   (fact:dat) -> case cmd of
                                   "fact" -> return $ getFact factFM fact
                                   "fact-set" -> do writeMS $
						      M.insert fact
							       (unwords dat)
							       factFM
						    return $
                                                      "Fact recorded."
		                   _ -> return "Unknown command."
       ircPrivmsg target result


getFact :: M.Map String String -> String -> String
getFact fm fact =
  case M.lookup fact fm of
    Nothing -> "I know nothing about " ++ fact ++ "."
    Just x  -> fact ++ ": " ++ x ++ "."
