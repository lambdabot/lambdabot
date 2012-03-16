{-# LANGUAGE TemplateHaskell, TypeFamilies, PatternGuards, ScopedTypeVariables #-}
-- | A todo list
--
-- (c) 2005 Samuel Bronson
module Plugin.Todo (theModule) where

import Plugin
import Lambdabot.Message as Msg (Message, packNick, unpackNick, showNick)
import qualified Data.ByteString.Char8 as P

plugin "Todo"

-- A list of key/elem pairs with an ordering determined by its position in the list
type TodoState = [(P.ByteString, P.ByteString)]

instance Module TodoModule where
    type ModuleState TodoModule = TodoState
    
    moduleCmds = return
        [ (command "todo")
            { help = say "todo. List todo entries"
            , process = \args -> withMsg $ \msg -> do
                todoList <- readMS
                getTodo msg todoList args
            }
        , (command "todo-add")
            { help = say "todo-add <idea>. Add a todo entry"
            , process = addTodo
            }
        , (command "todo-delete")
            { privileged = True
            , help = say "todo-delete <n>. Delete a todo entry (for admins)"
            , process = delTodo
            }
        ]

    moduleDefState  _ = return ([] :: TodoState)
    moduleSerialize _ = Just assocListPackedSerial

-- | Print todo list
getTodo :: Msg.Message m => m -> TodoState -> String -> Cmd Todo ()
getTodo msg todoList [] = say (formatTodo msg todoList)
getTodo _ _ _           = say "@todo has no args, try @todo-add or @list todo"

-- | Pretty print todo list
formatTodo :: Msg.Message m => m -> [(P.ByteString, P.ByteString)] -> String
formatTodo _ [] = "Nothing to do!"
formatTodo msg todoList =
    unlines $ map (\(n::Int, (idea, nick_)) -> concat $
            [ show n,". ",Msg.showNick msg $ unpackNick nick_,": ",P.unpack idea ]) $
                zip [0..] todoList

-- | Add new entry to list
addTodo :: String -> Cmd Todo ()
addTodo rest = do
    sender <- fmap Msg.packNick getSender
    modifyMS (++[(P.pack rest, sender)])
    say "Entry added to the todo list"

-- | Delete an entry from the list
delTodo :: String -> Cmd Todo ()
delTodo rest
    | Just n <- readM rest = say =<< withMS (\ls write -> case () of
          _ | null ls -> return "Todo list is empty"
            | n > length ls - 1 || n < 0
            -> return (show n ++ " is out of range")
    
            | otherwise -> do
                write (map snd . filter ((/= n) . fst) . zip [0..] $ ls)
                let (a,_) = ls !! n
                return ("Removed: " ++ P.unpack a))

    | otherwise = say "Syntax error. @todo <n>, where n :: Int"
