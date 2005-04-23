--
-- | A todo list
-- 
-- (c) 2005 Samuel Bronson
--
module Plugins.Todo (theModule) where

import IRC
import Util         (listSerializer,readM)
import Data.Char    (isDigit)

newtype TodoModule = TodoModule ()

theModule :: MODULE
theModule = MODULE $ TodoModule ()

type TodoState = [(String, String)]

instance Module TodoModule TodoState where
    moduleHelp _ s = return $ case s of
        "todo"        -> "@todo, list todo entries"
        "todo-add"    -> "@todo-add <idea>, add a todo entry"
        "todo-delete" -> "@todo-delete <m>, delete a todo entry (for admins)" 
        _ -> "Keep a todo list. Provides @todo, @todo-add, @todo-delete"

    moduleDefState  _ = return ([] :: [(String, String)])
    moduleSerialize _ = Just listSerializer
    
    moduleCmds _ = return ["todo", "todo-add", "todo-delete"] 
    process      _ msg source cmd rest =
        do todoList <- readMS
           case cmd of
               "todo"        -> getTodo source todoList rest
               "todo-add"    -> addTodo source sender rest
               "todo-delete" -> checkPrivs msg source (delTodo source rest)
               _ -> error "unimplemented command"
	where sender = ircNick msg

-- | Print todo list
getTodo :: String -> TodoState -> String -> ModuleT TodoState IRC ()
getTodo source todoList "" = 
    ircPrivmsg source (formatTodo todoList)
getTodo _      _        _  =
    error "@todo given arguments, try @todo-add or @listcommands todo"
 
-- | Pretty print todo list
formatTodo :: [(String, String)] -> String
formatTodo [] = "Nothing to do!"
formatTodo todoList =
    unlines $ map (\(idea, nick) -> "  * "++idea++" (submitted by "++nick++")") todoList

-- | Add new entry to list
addTodo :: String -> String -> String -> ModuleT TodoState IRC ()
addTodo source sender rest = do 
    modifyMS (++[(rest, sender)])
    ircPrivmsg source "Entry added to the todo list"        

-- | Delete an entry from the list
delTodo :: String -> String -> ModuleT TodoState IRC ()
delTodo source rest | rest /= [] && all isDigit rest = do
    n  <- readM rest
    ls <- readMS
    case () of {_
        | ls == [] -> ircPrivmsg source "Todo list is empty"
        | n > length ls - 1 || n < 0
        -> ircPrivmsg source "Index to @todo-del is out of range"

        | otherwise -> do 
            modifyMS (map snd . filter ((/= n) . fst) . zip [0..])
            let (a,b) = ls !! n
            ircPrivmsg source $ "Removed item: " ++ a ++ ", " ++ b
    }

delTodo source _ = ircPrivmsg source "Syntax error. @todo <n>, where n :: Int"
