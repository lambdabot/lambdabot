--
-- | Karma
--
module Plugins.Todo (theModule) where

import IRC
import Util (stdSerializer)

import Control.Monad (liftM)

newtype TodoModule = TodoModule ()

theModule :: MODULE
theModule = MODULE $ TodoModule ()

type TodoState = [(String, String)]
--type Todo m a = ModuleT TodoState m a

instance Module TodoModule TodoState where
    moduleHelp _ "todo"        = return "list todo entries"
    moduleHelp _ "todo-add"    = return "add a todo entry"
    moduleHelp _ "todo-delete" = return "delete a todo entry (for admins)" 
    moduleHelp _ _             = return "keep a todo list"

    moduleDefState  _ = return ([] :: [(String, String)])
    moduleSerialize _ = Just stdSerializer
    
    moduleCmds _ = return ["todo", "todo-add", "todo-delete"] 
    process      _ msg source cmd rest =
        do todoList <- readMS
           case cmd of
               "todo"        -> getTodo source todoList
               "todo-add"    -> addTodo source sender rest
               "todo-delete" -> checkPrivs msg source (delTodo source rest)
               _ -> error "unimplemented command"
	where sender = ircNick msg

getTodo :: String -> [(String, String)] -> ModuleT TodoState IRC ()
getTodo source todoList = 
    ircPrivmsg source (formatTodo todoList)

formatTodo :: [(String, String)] -> String
formatTodo todoList =
    unlines
    ("Todo Entries"
     :(map (\(idea, nick) -> 
            ("  * "++idea++" (submitted by "++nick++")"))
       todoList)
    )

addTodo :: String -> String -> String -> ModuleT TodoState IRC ()
addTodo source sender rest = 
    do modifyMS (++[(rest, sender)])
       ircPrivmsg source "added todolist entry"        

delTodo :: String -> String -> ModuleT TodoState IRC ()
delTodo source rest =
    do let n = read rest
       item <- liftM (!! n) readMS 
       modifyMS (map snd . filter ((/= n) . fst) . zip [0..])
       ircPrivmsg source ("removed todo-item " ++ show item)
