
-- 	$Id: FactModule.hs,v 1.8 2003/07/29 13:03:02 eris Exp $

--
-- copyright (c) 2003 Shae Erisson
-- license: lGPL
-- quick ugly hack to get factoids in lambdabot
-- requires Database.PostgreSQL.HSQL from
-- http://sf.net/projects/htoolkit/
-- 
-- assumes a postgresql database with a table named fact and fields named fact and value
--

module FactModule (theModule) where

import IRC
import Util

import Data.List
import Data.Char
import Control.Monad.Reader
import Database.PostgreSQL.HSQL

newtype FactModule = FactModule ()

theModule = MODULE factModule
factModule = FactModule ()

instance Module FactModule where
    moduleName   m = return "fact"
    commands     m = return ["fact"]
    process      m _ target cmd rest
                 = do -- thanks Marvin--
                   x <- liftIO (factoid rest)
                   ircPrivmsg target x

-- note the handy error catching, now with 15% less crashes!
factoid                     :: String -> IO String
factoid rest
 | length (words rest) > 1  = if (factKey == "delete") then catchSql (factDel factVal) sqlHandler
                              else catchSql (factSet factKey factVal) sqlHandler
 | length (words rest) == 1 =  catchSql (factGet factKey) sqlHandler
 | otherwise                = return "empty factoid, BZZZT, thank you for playing!"
    where
    factKey = lowerCaseString $ clean $ head $ words rest
    factVal = clean $ snd $ break (== ' ') rest

-- postgresql stuff under here
dbhost = "localhost"
username = "shae"
database = "shae"
password = "password"

connectGet = connect dbhost username database password

sqlHandler se = do putStrLn (show se)
                   return (show se)

getFVV :: Statement -> IO [Char]
getFVV = flip getFieldValue "value"

factGet   :: [Char] -> IO [Char]
factGet x =  do connection <- connectGet
                statement  <- query connection (fact_get_query x)
                results <- liftM concat $ liftM (intersperse ", ") $ collectRows getFVV statement
                disconnect connection
                if (results == "") then return "Nothing" else return results

factSet :: [Char] -> [Char] -> IO [Char]
factSet x y = do connection <- connectGet
                 execute connection $ fact_set_query x y
                 disconnect connection
                 return ("set " ++ x ++ " to " ++ y)

factDel   :: [Char] -> IO [Char]
factDel x = do connection <- connectGet
               execute connection $ fact_del_query x
               disconnect connection
               return ("deleted " ++ x)

fact_get_query   :: [Char] -> [Char]
fact_get_query x =  "select * from fact where fact = '" ++ x ++ "';"

fact_del_query   :: [Char] -> [Char]
fact_del_query x = "delete from fact where fact = '" ++ x ++ "';"

fact_set_query :: [Char] -> [Char] -> [Char]
fact_set_query x y = concat ["INSERT INTO fact ( fact, value ) VALUES ('",x,"','",y,"');"]



clean = unticker . trim
-- single tick escaping code from Igloo
unticker str = concatMap untick str

-- extra escaping code from tmoertel
untick x | x `elem` specials = ['\\',x]
         | otherwise         = [x]
         where
         specials = "\\\'"
-- trim whitespace from the front and back of the string
trim str = reverse $ dropS $ reverse $ dropS str
           where dropS = dropWhile isSpace
