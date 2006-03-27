--
-- | A todo list
-- 
-- (c) 2005 Samuel Bronson
--
module Plugins.Todo (theModule) where

import Lambdabot
import LBState
import qualified IRC
import Serial
import qualified Data.FastPackedString as P

newtype TodoModule = TodoModule ()

theModule :: MODULE
theModule = MODULE $ TodoModule ()

-- A list of key/elem pairs with an ordering determined by its position in the list
type TodoState = [(P.FastString, P.FastString)]

instance Module TodoModule TodoState where
    moduleCmds  _ = ["todo", "todo-add"] 
    modulePrivs _ = ["todo-delete"]
    moduleHelp _ s = case s of
        "todo"        -> "todo. List todo entries"
        "todo-add"    -> "todo-add <idea>. Add a todo entry"
        "todo-delete" -> "todo-delete <n>. Delete a todo entry (for admins)" 
        _ -> "Keep a todo list. Provides @todo, @todo-add, @todo-delete"

    moduleDefState  _ = return ([] :: TodoState)
    moduleSerialize _ = Just assocListPackedSerial
    
    process _ msg _ cmd rest = do
       todoList <- readMS
       case cmd of
           "todo"        -> getTodo todoList rest
           "todo-add"    -> addTodo sender rest
           "todo-delete" -> delTodo rest

        where sender = IRC.nick msg

-- | Print todo list
getTodo :: TodoState -> String -> ModuleLB TodoState
getTodo todoList [] = return [formatTodo todoList]
getTodo _ _         = error "@todo has no args, try @todo-add or @listcommands todo"
 
-- | Pretty print todo list
formatTodo :: [(P.FastString, P.FastString)] -> String
formatTodo [] = "Nothing to do!"
formatTodo todoList =
    unlines $ map (\(n::Int, (idea, nick)) -> concat $ 
            [ show n,". ",P.unpack nick,": ",P.unpack idea ]) $
                zip [0..] todoList 

-- | Add new entry to list
addTodo :: String -> String -> ModuleLB TodoState
addTodo sender rest = do 
    modifyMS (++[(P.pack rest, P.pack sender)])
    return ["Entry added to the todo list"]

-- | Delete an entry from the list
delTodo :: String -> ModuleLB TodoState
delTodo rest 
    | Just n <- readM rest = withMS $ \ls write -> case () of   
      _ | null ls -> return ["Todo list is empty"]
        | n > length ls - 1 || n < 0
        -> return [show n ++ " is out of range"]

        | otherwise -> do 
            write (map snd . filter ((/= n) . fst) . zip [0..] $ ls)
            let (a,_) = ls !! n
            return ["Removed: " ++ P.unpack a]

    | otherwise = return ["Syntax error. @todo <n>, where n :: Int"]
