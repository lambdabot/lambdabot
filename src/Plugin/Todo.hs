{-# LANGUAGE TemplateHaskell, TypeFamilies, PatternGuards, ScopedTypeVariables #-}
-- | A todo list
--
-- (c) 2005 Samuel Bronson
module Plugin.Todo (theModule) where

import Plugin
import Lambdabot.Message as Msg (Message, nick, packNick, unpackNick, showNick)
import qualified Data.ByteString.Char8 as P

$(plugin "Todo")

-- A list of key/elem pairs with an ordering determined by its position in the list
type TodoState = [(P.ByteString, P.ByteString)]

instance Module TodoModule where
    type ModuleState TodoModule = TodoState
    
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
           "todo"        -> getTodo msg todoList rest
           "todo-add"    -> addTodo sender rest
           "todo-delete" -> delTodo rest

        where sender = Msg.packNick $ Msg.nick msg

-- | Print todo list
getTodo :: Msg.Message m => m -> TodoState -> String -> Todo [String]
getTodo msg todoList [] = return [formatTodo msg todoList]
getTodo _ _ _           = error "@todo has no args, try @todo-add or @list todo"

-- | Pretty print todo list
formatTodo :: Msg.Message m => m -> [(P.ByteString, P.ByteString)] -> String
formatTodo _ [] = "Nothing to do!"
formatTodo msg todoList =
    unlines $ map (\(n::Int, (idea, nick_)) -> concat $
            [ show n,". ",showNick msg $ unpackNick nick_,": ",P.unpack idea ]) $
                zip [0..] todoList

-- | Add new entry to list
addTodo :: P.ByteString -> String -> Todo [String]
addTodo sender rest = do
    modifyMS (++[(P.pack rest, sender)])
    return ["Entry added to the todo list"]

-- | Delete an entry from the list
delTodo :: String -> Todo [String]
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
