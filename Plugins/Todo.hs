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
    moduleHelp _ s = return $ case s of
        "todo"        -> "@todo, list todo entries"
        "todo-add"    -> "@todo-add <idea>, add a todo entry"
        "todo-delete" -> "@todo-delete <m>, delete a todo entry (for admins)" 
        _ -> "Keep a todo list. Provides @todo, @todo-add, @todo-delete"

    moduleDefState  _ = return ([] :: TodoState)
    moduleSerialize _ = Just assocListPackedSerial
    
    moduleCmds  _ = return ["todo", "todo-add"] 
    modulePrivs _ = return ["todo-delete"]
    process      _ msg source cmd rest =
        do todoList <- readMS
           case cmd of
               "todo"        -> getTodo source todoList rest
               "todo-add"    -> addTodo source sender rest
               "todo-delete" -> delTodo source rest
               _ -> error "unimplemented command"
	where sender = IRC.nick msg

-- | Print todo list
getTodo :: String -> TodoState -> String -> ModuleT TodoState LB ()
getTodo source todoList "" = ircPrivmsg source (formatTodo todoList)
getTodo _ _ _ = error "@todo has no args, try @todo-add or @listcommands todo"
 
-- | Pretty print todo list
formatTodo :: [(P.FastString, P.FastString)] -> String
formatTodo [] = "Nothing to do!"
formatTodo todoList =
    unlines $ map (\(n::Int, (idea, nick)) -> concat $ 
            [ show n,". ",P.unpack nick,": ",P.unpack idea ]) $
                zip [0..] todoList 

-- | Add new entry to list
addTodo :: String -> String -> String -> ModuleT TodoState LB ()
addTodo source sender rest = do 
    modifyMS (++[(P.pack rest, P.pack sender)])
    ircPrivmsg source "Entry added to the todo list"        

-- | Delete an entry from the list
delTodo :: String -> String -> ModuleT TodoState LB ()
delTodo source rest 
    | Just n <- readM rest = withMS $ \ls write -> case () of   
      _ | null ls -> ircPrivmsg source "Todo list is empty"
        | n > length ls - 1 || n < 0
        -> ircPrivmsg source $ show n ++ " is out of range"

        | otherwise -> do 
            write (map snd . filter ((/= n) . fst) . zip [0..] $ ls)
            let (a,_) = ls !! n
            ircPrivmsg source $ "Removed: " ++ P.unpack a

    | otherwise = ircPrivmsg source "Syntax error. @todo <n>, where n :: Int"
