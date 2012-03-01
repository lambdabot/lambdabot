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
    
    moduleCmds _ =
        [ (command "todo")
            { help = say "todo. List todo entries"
            , process = \args -> withMsg $ \msg -> do
                todoList <- lift readMS
                lift (getTodo msg todoList args) >>= mapM_ say
            }
        , (command "todo-add")
            { help = say "todo-add <idea>. Add a todo entry"
            , process = \args -> withMsg $ \msg -> do
                let sender = Msg.packNick $ Msg.nick msg
                lift (addTodo sender args) >>= mapM_ say
            }
        , (command "todo-delete")
            { privileged = True
            , help = say "todo-delete <n>. Delete a todo entry (for admins)"
            , process = \args -> lift (delTodo args) >>= mapM_ say
            }
        ]

    moduleDefState  _ = return ([] :: TodoState)
    moduleSerialize _ = Just assocListPackedSerial

-- | Print todo list
getTodo :: Msg.Message m => m -> TodoState -> String -> Todo [String]
getTodo msg todoList [] = return [formatTodo msg todoList]
getTodo _ _ _           = error "@todo has no args, try @todo-add or @list todo"

-- | Pretty print todo list
formatTodo :: Msg.Message m => m -> [(P.ByteString, P.ByteString)] -> String
formatTodo _ [] = "Nothing to do!"
formatTodo msg todoList =
    unlines $ map (\(n::Int, (idea, nick_)) -> concat $
            [ show n,". ",Msg.showNick msg $ unpackNick nick_,": ",P.unpack idea ]) $
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
