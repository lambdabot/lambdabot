{-# LANGUAGE PatternGuards #-}
-- | A todo list
--
-- (c) 2005 Samuel Bronson
module Lambdabot.Plugin.Todo (theModule) where

import Lambdabot.Compat.PackedNick
import Lambdabot.Plugin
import Control.Monad
import qualified Data.ByteString.Char8 as P

-- A list of key/elem pairs with an ordering determined by its position in the list
type TodoState = [(P.ByteString, P.ByteString)]
type Todo = ModuleT TodoState LB

theModule :: Module TodoState
theModule = newModule
    { moduleDefState  = return ([] :: TodoState)
    , moduleSerialize = Just assocListPackedSerial

    , moduleCmds = return
        [ (command "todo")
            { help = say "todo. List todo entries"
            , process = getTodo
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
    }

-- | Print todo list
getTodo :: String -> Cmd Todo ()
getTodo [] = readMS >>= sayTodo
getTodo _  = say "@todo has no args, try @todo-add or @list todo"

-- | Pretty print todo list
sayTodo :: [(P.ByteString, P.ByteString)] -> Cmd Todo ()
sayTodo [] = say "Nothing to do!"
sayTodo todoList = say . unlines =<< zipWithM fmtTodoItem ([0..] :: [Int]) todoList
    where
        fmtTodoItem n (idea, nick_) = do
            nick <- showNick (unpackNick nick_)
            return $ concat $
                [ show n,". ", nick ,": ",P.unpack idea ]

-- | Add new entry to list
addTodo :: String -> Cmd Todo ()
addTodo rest = do
    sender <- fmap packNick getSender
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
